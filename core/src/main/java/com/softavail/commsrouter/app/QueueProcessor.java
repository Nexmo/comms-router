package com.softavail.commsrouter.app;

import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.AssignmentRejectedException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.TaskEventHandler;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Optional;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import javax.persistence.EntityManager;

/**
 * Created by @author mapuo on 17.10.17.
 */
public class QueueProcessor {

  private static final Logger LOGGER = LogManager.getLogger(QueueProcessor.class);

  private static final long PROCESS_RETRY_DELAY_SECONDS = 10;

  private final String queueId;
  private final JpaDbFacade db;
  private final EntityMappers mappers;
  private final TaskDispatcher taskDispatcher;
  private final TaskEventHandler taskEventHandler;
  private final ScheduledThreadPoolExecutor threadPool;
  private final StateChangeListener stateChangeListener;

  private QueueProcessorState state;

  public QueueProcessor(String queueId, JpaDbFacade db, EntityMappers mappers,
      TaskDispatcher taskDispatcher, TaskEventHandler taskEventHandler,
      ScheduledThreadPoolExecutor threadPool, StateChangeListener stateChangeListener) {

    this.queueId = queueId;
    this.db = db;
    this.mappers = mappers;
    this.taskDispatcher = taskDispatcher;
    this.taskEventHandler = taskEventHandler;
    this.threadPool = threadPool;
    this.stateChangeListener = stateChangeListener;
    this.state = QueueProcessorState.IDLE;
  }

  public QueueProcessor(String queueId, JpaDbFacade db, EntityMappers mappers,
      TaskEventHandler taskEventHandler, ScheduledThreadPoolExecutor threadPool,
      TaskDispatcher taskDispatcher) {

    this(queueId, db, mappers, taskDispatcher, taskEventHandler, threadPool, null);
  }

  public String getQueueId() {
    return queueId;
  }

  public synchronized void process() {
    LOGGER.debug("Queue processor {}: process in {}", queueId, state);
    switch (state) {
      case IDLE:
        changeState(QueueProcessorState.CONSUME);
        threadPool.submit(this::processQueue);
        break;
      case CONSUME:
        changeState(QueueProcessorState.MUST_CONSUME);
        break;
      case MUST_CONSUME:
      default:
        break;
    }
  }

  private synchronized boolean tryComplete() {
    LOGGER.debug("Queue processor {}: complete in {}", queueId, state);
    switch (state) {
      case MUST_CONSUME:
        changeState(QueueProcessorState.CONSUME);
        return false;
      case CONSUME:
        changeState(QueueProcessorState.IDLE);
        return true;
    }
    LOGGER.error("Queue processor {}: invalid complete state: {}", queueId, state);
    throw new RuntimeException("Queue processor " + queueId + ": invalid compelte state: " + state);
  }

  private void changeState(QueueProcessorState newState) {
    LOGGER.debug("Queue processor {}:  change {} => {}", queueId, state, newState);
    QueueProcessorState oldState = state;
    state = newState;
    if (stateChangeListener != null) {
      StateChangeEvent changeEvent = new StateChangeEvent(queueId, oldState, newState);
      stateChangeListener.stateChanged(changeEvent);
    }
  }

  public synchronized boolean isWorking() {
    return state == QueueProcessorState.CONSUME || state == QueueProcessorState.MUST_CONSUME;
  }

  private void processQueue() {

    for (;;) {
      Optional<TaskAssignmentDto> taskAssignmentDto;
      try {
        taskAssignmentDto = db.transactionManager.executeWithLockRetry(this::getAssignment);
      } catch (CommsRouterException | RuntimeException e) {
        // Failed to get assignment. Most porbably DB is down, so let's try again a bit later.
        LOGGER.error("Queue processor {}: failure getting assignment: {}", e, e);
        // @todo make this wait time configurable
        threadPool.schedule(this::processQueue, PROCESS_RETRY_DELAY_SECONDS, TimeUnit.SECONDS);
        return;
      }

      if (!taskAssignmentDto.isPresent()) {
        // No task or no agent, try to compelte.
        if (tryComplete()) {
          return;
        }
        // Could not complete: somebody wants us to work more, so try one more loop.
        continue;
      }

      handleTaskAssignment(taskAssignmentDto.get());
    }

  }

  private void handleTaskAssignment(TaskAssignmentDto taskAssignmentDto) {
    threadPool.submit(() -> {
      try {
        taskEventHandler.onTaskAssigned(taskAssignmentDto);
      } catch (AssignmentRejectedException e) {
        // The handler has issued AssignmentRejectedException, so we should cancel the assignment
        taskDispatcher.rejectAssignment(taskAssignmentDto.getTask().getId());
      } catch (RuntimeException ex) {
        LOGGER.error("Queue {}: task assignment callback failure: {}", queueId, ex, ex);
        // TODO Implement some backoff retry with exponential time
      }
    });
  }

  @SuppressWarnings("unchecked")
  private Optional<TaskAssignmentDto> getAssignment(EntityManager em) throws CommsRouterException {

    return db.queue.findAssignment(em, queueId).map(matchResult -> {
      Agent agent = matchResult.agent;
      Task task = matchResult.task;
      // Assign
      agent.setState(AgentState.busy);
      task.setState(TaskState.assigned);
      task.setAgent(agent);

      TaskDto taskDto = mappers.task.toDto(task);
      AgentDto agentDto = mappers.agent.toDto(agent);
      return new TaskAssignmentDto(taskDto, agentDto);
    });
  }

}
