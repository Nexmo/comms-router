package com.softavail.commsrouter.app;

import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
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
  private final ScheduledThreadPoolExecutor threadPool;
  private final StateChangeListener stateChangeListener;

  private QueueProcessorState state;

  public QueueProcessor(
      String queueId,
      JpaDbFacade db,
      EntityMappers mappers,
      TaskDispatcher taskDispatcher,
      ScheduledThreadPoolExecutor threadPool,
      StateChangeListener stateChangeListener) {

    this.queueId = queueId;
    this.db = db;
    this.mappers = mappers;
    this.taskDispatcher = taskDispatcher;
    this.threadPool = threadPool;
    this.stateChangeListener = stateChangeListener;
    this.state = QueueProcessorState.IDLE;
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
      default:
    }
    LOGGER.error("Queue processor {}: invalid complete state: {}", queueId, state);
    throw new RuntimeException(
        "Queue processor " + queueId + ": invalid complete state: " + state);
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

    for (; ; ) {
      Optional<TaskAssignmentDto> taskAssignmentDto;
      try {
        taskAssignmentDto = db.transactionManager.executeWithLockRetry(this::getAssignment);
      } catch (CommsRouterException | RuntimeException e) {
        // Failed to get assignment. Most probably DB is down, so let's try again a bit later.
        LOGGER.error("Queue processor {}: failure getting assignment: {}", e, e);
        // @todo make this wait time configurable
        threadPool.schedule(this::processQueue, PROCESS_RETRY_DELAY_SECONDS, TimeUnit.SECONDS);
        return;
      }

      if (!taskAssignmentDto.isPresent()) {
        // No task or no agent, try to complete.
        if (tryComplete()) {
          return;
        }
        // Could not complete: somebody wants us to work more, so try one more loop.
        continue;
      }

      taskDispatcher.submitTaskAssignment(taskAssignmentDto.get());
    }

  }

  @SuppressWarnings("unchecked")
  private Optional<TaskAssignmentDto> getAssignment(EntityManager em)
      throws CommsRouterException {

    return db.queue.findAssignment(em, queueId)
        .map(matchResult -> {
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

  public static class Builder {

    private String queueId;
    private JpaDbFacade db;
    private EntityMappers mappers;
    private TaskDispatcher taskDispatcher;
    private ScheduledThreadPoolExecutor threadPool;
    private StateChangeListener stateChangeListener = null;

    public Builder setQueueId(String queueId) {
      this.queueId = queueId;
      return this;
    }

    public Builder setDb(JpaDbFacade db) {
      this.db = db;
      return this;
    }

    public Builder setMappers(EntityMappers mappers) {
      this.mappers = mappers;
      return this;
    }

    public Builder setTaskDispatcher(TaskDispatcher taskDispatcher) {
      this.taskDispatcher = taskDispatcher;
      return this;
    }

    public Builder setThreadPool(ScheduledThreadPoolExecutor threadPool) {
      this.threadPool = threadPool;
      return this;
    }

    public Builder setStateChangeListener(StateChangeListener stateChangeListener) {
      this.stateChangeListener = stateChangeListener;
      return this;
    }

    public QueueProcessor build() {
      return new QueueProcessor(queueId, db, mappers, taskDispatcher, threadPool,
          stateChangeListener);
    }
  }

}
