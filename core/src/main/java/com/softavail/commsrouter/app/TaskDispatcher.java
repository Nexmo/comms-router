/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.app;

import com.google.common.collect.Maps;

import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.TaskEventHandler;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import javax.persistence.EntityManager;
import javax.persistence.LockModeType;

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
      EntityMappers dtoMappers,
      int threadPoolSize) {

    this.db = db;
    this.mappers = dtoMappers;
    this.taskEventHandler = taskEventHandler;
    this.threadPool = new ScheduledThreadPoolExecutor(threadPoolSize);
    this.threadPool.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
    startQueueProcessors();
  }

  @SuppressWarnings("unchecked")
  private void startQueueProcessors() {
    try {
      db.transactionManager.executeVoid(em ->
          db.router.list(em).stream()
              .map(router -> db.queue.list(em, router.getId()))
              .flatMap(Collection::stream)
              .map(Queue::getId)
              .map(this::createQueueProcessor)
              .forEach(QueueProcessor::process));
    } catch (CommsRouterException e) {
      throw new RuntimeException("Can not instantiate TaskDispatcher!", e);
    }
  }

  private synchronized QueueProcessor createQueueProcessor(String queueId) {
    Optional.ofNullable(SCHEDULED_FUTURES.get(queueId))
        .ifPresent(schedule -> schedule.cancel(DO_NOT_INTERRUPT_IF_RUNNING));
    QueueProcessor queueProcessor = QUEUE_PROCESSORS.get(queueId);
    if (queueProcessor == null) {
      queueProcessor =
          new QueueProcessor(
              queueId,
              db,
              mappers,
              this,
              taskEventHandler,
              threadPool,
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
    threadPool.submit(() -> {
      try {
        doDispatchAgent(agentId);
      } catch (Exception ex) {
        LOGGER.error("Dispatch agent: {}: failure: {}", agentId, ex, ex);
      }
    });
  }

  @SuppressWarnings("unchecked")
  private void doDispatchAgent(String agentId)
      throws CommsRouterException {

    TaskAssignmentDto taskAssignment = db.transactionManager.executeWithLockRetry((em) -> {

      String routerId = db.agent.get(em, agentId).getRouterId();
      em.find(Router.class, routerId, LockModeType.PESSIMISTIC_WRITE);

      String qlString = "SELECT t, a FROM Task t " + "JOIN t.queue q JOIN q.agents a "
          + "WHERE a.id = :agentId and a.state = :agentState and t.state = :taskState "
          + "ORDER BY t.priority DESC";

      List<Object[]> result = em.createQuery(qlString)
          .setParameter("agentId", agentId)
          .setParameter("agentState", AgentState.ready)
          .setParameter("taskState", TaskState.waiting)
          .setMaxResults(1)
          .getResultList();

      if (result.isEmpty()) {
        return null;
      }

      Task task = (Task) result.get(0)[0];
      Agent agent = (Agent) result.get(0)[1];

      assignTask(task, agent);
      return new TaskAssignmentDto(mappers.task.toDto(task), mappers.agent.toDto(agent));
    });

    if (taskAssignment != null) {
      LOGGER.info("Dispatch agent {}: task {} assgined to agent {}", agentId,
          taskAssignment.getTask(), taskAssignment.getAgent());
      taskEventHandler.onTaskAssigned(taskAssignment);
    } else {
      LOGGER.info("Dispatch agent {}: no suitable task or agent already busy", agentId);
    }
  }

  private void assignTask(Task task, Agent agent) {
    agent.setState(AgentState.busy);
    task.setState(TaskState.assigned);
    task.setAgent(agent);
  }

  public Optional<String> rejectAssignment(EntityManager em, String taskId)
      throws NotFoundException {

    Task task = db.task.get(em, taskId);
    Agent agent = task.getAgent();

    if (task.getState().isAssigned() && agent != null) {
      agent.setState(AgentState.unavailable);
      task.setState(TaskState.waiting);
      task.setAgent(null);

      return Optional.of(task.getQueue().getId());
    }

    return Optional.empty();
  }

}
