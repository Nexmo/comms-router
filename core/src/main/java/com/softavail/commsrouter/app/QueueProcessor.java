package com.softavail.commsrouter.app;

import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
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
import javax.persistence.EntityManager;

/**
 * Created by @author mapuo on 17.10.17.
 */
public class QueueProcessor {

  private static final Logger LOGGER = LogManager.getLogger(QueueProcessor.class);

  private final String queueId;
  private final JpaDbFacade db;
  private final EntityMappers mappers;
  private final TaskEventHandler taskEventHandler;
  private final ScheduledThreadPoolExecutor threadPool;
  private final StateChangeListener stateChangeListener;

  private QueueProcessorState state;

  public QueueProcessor(
      String queueId,
      JpaDbFacade db,
      EntityMappers mappers,
      TaskEventHandler taskEventHandler,
      ScheduledThreadPoolExecutor threadPool,
      StateChangeListener stateChangeListener) {

    this.queueId = queueId;
    this.db = db;
    this.mappers = mappers;
    this.taskEventHandler = taskEventHandler;
    this.threadPool = threadPool;
    this.stateChangeListener = stateChangeListener;
    this.state = QueueProcessorState.IDLE;
  }

  public QueueProcessor(
      String queueId,
      JpaDbFacade db,
      EntityMappers mappers,
      TaskEventHandler taskEventHandler,
      ScheduledThreadPoolExecutor threadPool) {

    this(queueId, db, mappers, taskEventHandler, threadPool, null);
  }

  public String getQueueId() {
    return queueId;
  }

  public synchronized void process() {
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

  private synchronized void nextState() {
    switch (state) {
      case MUST_CONSUME:
        changeState(QueueProcessorState.CONSUME);
        break;
      case CONSUME:
        changeState(QueueProcessorState.IDLE);
        break;
      case IDLE:
      default:
        break;
    }
  }

  private void changeState(QueueProcessorState newState) {
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
    try {

      do {

        Optional<TaskAssignmentDto> taskAssignmentDto =
            db.transactionManager.executeWithLockRetry(this::getAssignment);

        if (taskAssignmentDto.isPresent()) {
          taskEventHandler.onTaskAssigned(taskAssignmentDto.get());
        } else {
          nextState();
        }

      } while (isWorking());

    } catch (Exception e) {
      LOGGER.error("Dispatch in Queue: {} failure: {}", queueId, e, e);
      // TODO Implement some backoff retry with exponential time
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

}
