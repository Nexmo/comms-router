/*
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.commsrouter.app;

import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
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

  private final Long routerId;
  private final Long queueId;
  private final JpaDbFacade db;
  private final TaskDispatcher taskDispatcher;
  private final ScheduledThreadPoolExecutor threadPool;
  private final long processRetryDelaySeconds;
  private final StateChangeListener stateChangeListener;

  private QueueProcessorState state;

  public QueueProcessor(
      Long routerId,
      Long queueId,
      JpaDbFacade db,
      TaskDispatcher taskDispatcher,
      ScheduledThreadPoolExecutor threadPool,
      long processRetryDelaySeconds,
      StateChangeListener stateChangeListener) {

    this.routerId = routerId;
    this.queueId = queueId;
    this.db = db;
    this.taskDispatcher = taskDispatcher;
    this.threadPool = threadPool;
    this.processRetryDelaySeconds = processRetryDelaySeconds;
    this.stateChangeListener = stateChangeListener;
    this.state = QueueProcessorState.IDLE;
  }

  public Long getQueueId() {
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
        taskAssignmentDto = db.transactionManager.executeWithLockRetry(em -> {
          db.router.lock(em, routerId);
          return getAssignment(em);
        });
      } catch (CommsRouterException | RuntimeException e) {
        // Failed to get assignment. Most probably DB is down, so let's try again a bit later.
        LOGGER.error("Queue processor {}: failure getting assignment: {}", queueId, e, e);
        threadPool.schedule(this::processQueue, processRetryDelaySeconds, TimeUnit.SECONDS);
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

    return db.queue.findAssignment(em, queueId).map(taskDispatcher::assignTask);
  }

  public static class Builder {

    private Long routerId;
    private Long queueId;
    private JpaDbFacade db;
    private TaskDispatcher taskDispatcher;
    private ScheduledThreadPoolExecutor threadPool;
    private long processRetryDelaySeconds;
    private StateChangeListener stateChangeListener = null;

    public Builder setRouterId(Long routerId) {
      this.routerId = routerId;
      return this;
    }

    public Builder setQueueId(Long queueId) {
      this.queueId = queueId;
      return this;
    }

    public Builder setDb(JpaDbFacade db) {
      this.db = db;
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

    public Builder setProcessRetryDelaySeconds(long processRetryDelaySeconds) {
      this.processRetryDelaySeconds = processRetryDelaySeconds;
      return this;
    }

    public Builder setStateChangeListener(StateChangeListener stateChangeListener) {
      this.stateChangeListener = stateChangeListener;
      return this;
    }

    public QueueProcessor build() {
      return new QueueProcessor(routerId, queueId, db, taskDispatcher, threadPool,
          processRetryDelaySeconds, stateChangeListener);
    }
  }

}
