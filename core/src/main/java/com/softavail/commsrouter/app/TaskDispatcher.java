/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.app;

import com.google.common.collect.Maps;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.TaskEventHandler;
import com.softavail.commsrouter.domain.ApiObject;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author ikrustev
 */
public class TaskDispatcher {

  private static final Logger LOGGER = LogManager.getLogger(TaskDispatcher.class);

  private static final Map<String, QueueProcessor> QUEUE_PROCESSORS = Maps.newHashMap();

  private static final Map<String, ScheduledFuture> SCHEDULED_FUTURES = Maps.newHashMap();
  private static final long EVICTION_DELAY_MINUTES = 10;
  private static final boolean DO_NOT_INTERRUPT_IF_RUNNING = false;

  private final JpaDbFacade db;
  private final EntityMappers mappers;
  private final TaskEventHandler taskEventHandler;
  private final ScheduledThreadPoolExecutor threadPool;

  public TaskDispatcher(
      JpaDbFacade db,
      TaskEventHandler taskEventHandler,
      EntityMappers dtoMappers)
      throws CommsRouterException {

    this.db = db;
    this.mappers = dtoMappers;
    this.taskEventHandler = taskEventHandler;
    // @todo: config threads count
    this.threadPool = new ScheduledThreadPoolExecutor(10);
    threadPool.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
    startQueueProcessors();
  }

  @SuppressWarnings("unchecked")
  private void startQueueProcessors()
      throws CommsRouterException {

    db.transactionManager.executeVoid(em ->
        db.router.list(em).stream()
            .map(router -> db.queue.list(em, router.getId()))
            .flatMap(Collection::stream)
            .map(Queue::getId)
            .map(this::createQueueProcessor)
            .forEach(QueueProcessor::process));
  }

  private synchronized QueueProcessor createQueueProcessor(String queueId) {
    Optional.ofNullable(SCHEDULED_FUTURES.get(queueId))
        .ifPresent(schedule -> schedule.cancel(DO_NOT_INTERRUPT_IF_RUNNING));
    QueueProcessor queueProcessor = QUEUE_PROCESSORS.get(queueId);
    if (queueProcessor == null) {
      queueProcessor = new QueueProcessor(queueId, db, mappers, taskEventHandler, threadPool,
          (StateIdleListener) this::handleStateChange);
      QUEUE_PROCESSORS.put(queueId, queueProcessor);
    }
    return queueProcessor;
  }

  private synchronized void removeQueueProcessor(String queueId) {
    QueueProcessor queueProcessor = QUEUE_PROCESSORS.get(queueId);
    if (!queueProcessor.isWorking()) {
      QUEUE_PROCESSORS.remove(queueId);
    }
  }

  private void handleStateChange(String queueId) {
    ScheduledFuture<?> schedule = threadPool.schedule(
        () -> removeQueueProcessor(queueId), EVICTION_DELAY_MINUTES, TimeUnit.MINUTES);
    SCHEDULED_FUTURES.put(queueId, schedule);
  }

  public void close() {
    // @todo: logs and config
    threadPool.shutdown();
    try {
      if (threadPool.awaitTermination(10, TimeUnit.SECONDS)) {
        LOGGER.info("Dispatcher thread pool down.");
      } else {
        LOGGER.warn("Dispatcher thread pool shutdown timeout. Forcing ...");
        threadPool.shutdownNow();
        if (threadPool.awaitTermination(10, TimeUnit.SECONDS)) {
          LOGGER.info("Dispatcher thread pool down after being forced.");
        } else {
          LOGGER.error("Dispatcher thread pool did not shut down.");
        }
      }
    } catch (InterruptedException ex) {
      LOGGER.warn("Interrupted while waiting for the dispatcher thread pool to go shut down.");
    }
  }

  public void dispatchQueue(String queueId) {
    createQueueProcessor(queueId).process();
  }

  public void dispatchAgent(String agentId) {
    try {
      // Get the queueId from the agent
      db.transactionManager.executeVoid(em ->
          db.agent.get(em, agentId).getQueues()
              .parallelStream()
              .map(ApiObject::getId)
              .map(this::createQueueProcessor)
              .forEach(QueueProcessor::process));
    } catch (CommsRouterException e) {
      LOGGER.error("Dispatch task {}: failure: {}", agentId, e, e);
    }
  }

}
