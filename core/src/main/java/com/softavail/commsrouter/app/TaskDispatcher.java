/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.app;

import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CallbackException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.TaskEventHandler;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Route;
import com.softavail.commsrouter.domain.Rule;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.domain.result.MatchResult;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import com.softavail.commsrouter.jpa.result.TaskEnumerableResult;
import com.softavail.commsrouter.util.Fields;

import net.jodah.failsafe.Failsafe;
import net.jodah.failsafe.RetryPolicy;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class TaskDispatcher {

  private static final Logger LOGGER = LogManager.getLogger(TaskDispatcher.class);

  private static final Map<String, ScheduledFuture<?>> scheduledTaskTimers = new HashMap<>();

  private final JpaDbFacade db;
  private final EntityMappers mappers;
  private final TaskEventHandler taskEventHandler;
  private final ScheduledThreadPoolExecutor threadPool;
  private final CoreConfiguration configuration;
  private final QueueProcessorManager queueProcessorManager;
  private final RetryPolicy retryPolicy;

  public TaskDispatcher(JpaDbFacade db, EntityMappers mappers, TaskEventHandler taskEventHandler) {
    this(db, mappers, CoreConfiguration.DEFAULT, taskEventHandler);
  }

  public TaskDispatcher(JpaDbFacade db, EntityMappers mappers, CoreConfiguration configuration,
      TaskEventHandler taskEventHandler) {
    this.configuration = configuration;
    this.db = db;
    this.mappers = mappers;
    this.taskEventHandler = taskEventHandler;
    this.threadPool = new ScheduledThreadPoolExecutor(configuration.getDispatcherThreadPoolSize());
    this.threadPool.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
    this.queueProcessorManager = QueueProcessorManager.getInstance();
    this.retryPolicy = new RetryPolicy()
        .retryOn(CallbackException.class)
        .retryOn(RuntimeException.class)
        .withBackoff(configuration.getBackoffDelay(), configuration.getBackoffDelayMax(),
            TimeUnit.SECONDS)
        .withJitter(configuration.getJitter(), TimeUnit.MILLISECONDS);
    startQueueProcessors();
    restartWaitingTaskTimers();
  }

  @SuppressWarnings("unchecked")
  private void startQueueProcessors() {
    try {
      db.transactionManager.executeVoid(em ->
          db.router.list(em).stream()
              .map(router -> db.queue.list(em, router.getId()))
              .flatMap(Collection::stream)
              .map(Queue::getId)
              .forEach(this::process));
    } catch (CommsRouterException e) {
      throw new RuntimeException("Can not instantiate TaskDispatcher!", e);
    }
  }

  private void process(String queueId) {
    queueProcessorManager
        .processQueue(queueId, db, mappers, this, configuration, threadPool);
  }

  public void close() {
    // @todo: logs
    threadPool.shutdown();
    try {
      final Integer shutdownDelay = configuration.getDispatcherThreadShutdownDelay();
      if (threadPool.awaitTermination(shutdownDelay, TimeUnit.SECONDS)) {
        LOGGER.info("Dispatcher thread pool down.");
      } else {
        LOGGER.warn("Dispatcher thread pool shutdown timeout. Forcing ...");
        threadPool.shutdownNow();
        if (threadPool.awaitTermination(shutdownDelay, TimeUnit.SECONDS)) {
          LOGGER.info("Dispatcher thread pool down after being forced.");
        } else {
          LOGGER.error("Dispatcher thread pool did not shut down.");
        }
      }
    } catch (InterruptedException ex) {
      LOGGER.warn(
          "Interrupted while waiting for the dispatcher thread pool to go shut down.");
    }
  }

  public void dispatchTask(TaskDto taskDto) {
    process(taskDto.getQueueId());
    startTaskTimer(taskDto);
  }

  public void dispatchAgent(String agentId) {
    threadPool.submit(() -> {
      try {
        doDispatchAgent(agentId);
      } catch (RuntimeException | CommsRouterException e) {
        LOGGER.error("Dispatch agent {}: failure: {}", agentId, e, e);
      }
    });
  }

  private void doDispatchAgent(String agentId) throws CommsRouterException {
    TaskAssignmentDto taskAssignmentDto = db.transactionManager.executeWithLockRetry((em) -> {
      MatchResult matchResult = db.queue.findAssignmentForAgent(em, agentId);

      if (matchResult == null) {
        return null;
      }

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
    if (taskAssignmentDto != null) {
      submitTaskAssignment(taskAssignmentDto);
    }
  }

  public void submitTaskAssignment(TaskAssignmentDto taskAssignmentDto) {
    Failsafe.with(retryPolicy).with(threadPool)
        .onSuccess((ignored, executionContext) ->
            LOGGER.debug("Task {} assigned to agent {}",
                taskAssignmentDto.getTask(), taskAssignmentDto.getAgent()))
        .onRetry((result, failure, context) ->
            LOGGER.warn("Retry assigning task {} to agent {}: {}, {}",
                taskAssignmentDto.getTask(), taskAssignmentDto.getAgent(), failure, context))
        .onFailure((ignored, throwable) ->
            LOGGER.error("Failure assigning task {} to agent {}: {}",
                taskAssignmentDto.getTask(), taskAssignmentDto.getAgent(), throwable, throwable))
        .run(() -> taskEventHandler.onTaskAssigned(taskAssignmentDto));
  }

  private void startTaskTimer(TaskDto taskDto) {

    LOGGER.debug("Starting wait timer for task {}", taskDto.getId());

    ScheduledFuture<?> timer = threadPool.schedule(() -> {
      onQueuedTaskTimeout(taskDto.getId());
    }, taskDto.getQueuedTimeout(), TimeUnit.SECONDS);

    scheduledTaskTimers.put(taskDto.getId(), timer);
  }

  private void onQueuedTaskTimeout(String taskId) {

    try {
      LOGGER.debug("onQueuedTaskTimeout(): Task with ID='{}' timed-out", taskId);
      processTaskTimeout(taskId);
    } catch (RuntimeException | CommsRouterException ex) {
      LOGGER.error("Exception while processing timeout for task {}: {}", 
          taskId, ex);
    }
  }

  private void processTaskTimeout(String taskId)
      throws CommsRouterException {

    TaskDto taskDto = db.transactionManager.execute((em) -> {
      Task task = db.task.get(em, taskId);
      if (null == task.getState()) {
        return null;
      }

      switch (task.getState()) {
        case completed:
          return null;
        case assigned:
          return null;
        case waiting: {
          Route matchedRoute;
          Rule rule = task.getRule();
          if (rule != null) {
            matchedRoute = getNextRoute(task.getRule(), task.getCurrentRoute().getId());
          } else {
            // default route
            Fields.nullify(task::setExpirationDate, task.getExpirationDate());
            return null;
          }

          if (matchedRoute == null) {
            Fields.nullify(task::setExpirationDate, task.getExpirationDate());
            return null;
          }

          Fields.update(task::setCurrentRoute, task.getCurrentRoute(), matchedRoute);

          if (matchedRoute.getPriority() != null) {
            Fields.update(
                task::setPriority, task.getPriority(), matchedRoute.getPriority());
          }
          
          if (matchedRoute.getTimeout() != null) {
            Fields.update(task::setQueuedTimeout, task.getQueuedTimeout(),
                matchedRoute.getTimeout());

            if (matchedRoute.getTimeout() > 0) {
              Date expirationDate =
                  new Date(System.currentTimeMillis() + matchedRoute.getTimeout() * 1000);
              LOGGER.trace("Next route, update expirationDate:{} for task:{} ", expirationDate,
                  task.getId());
              Fields.update(task::setExpirationDate, task.getExpirationDate(), expirationDate);
            } else {
              LOGGER.trace("Next route, clear expirationDate for task:{}", task.getId());
              Fields.nullify(task::setExpirationDate, task.getExpirationDate());
            }
          } else {
            if (task.getQueuedTimeout() != null && task.getQueuedTimeout() > 0) {
              Date expirationDate =
                  new Date(System.currentTimeMillis() + task.getQueuedTimeout() * 1000);
              LOGGER.trace("Default, update expirationDate:{} for task:{} ", expirationDate,
                  task.getId());
              Fields.update(task::setExpirationDate, task.getExpirationDate(), expirationDate);
            } else {
              LOGGER.trace("Default, clear expirationDate for task:{}", task.getId());
              Fields.nullify(task::setExpirationDate, task.getExpirationDate());
            }
          }
          
          if (matchedRoute.getQueueId() != null) {
            RouterObjectId objectId = RouterObjectId.builder()
                .setId(matchedRoute.getQueueId())
                .setRouterId(task.getRouterId())
                .build();
            Queue queue = db.queue.get(em, objectId);
            Fields.update(task::setQueue, task.getQueue(), queue);
            if (!task.getQueue().getId().equals(matchedRoute.getQueueId())) {
              Fields.update(task::setAgent, task.getAgent(), null);
            }
          }
        }
          break;
        default:
          return null;
      }
      return mappers.task.toDto(task);
    });

    if (taskDto != null) {
      startTaskTimer(taskDto);
    }

  }

  public Route getNextRoute(Rule rule, Long prevRouteId) {
    if (rule != null) {
      List<Route> routes = rule.getRoutes();
      boolean stopOnNextIteration = false;
      for (Route route : routes) {
        if (stopOnNextIteration) {
          return route;
        }
        if (Objects.equals(route.getId(), prevRouteId)) {
          stopOnNextIteration = true;
        }
      }
      return null;
    } else {
      LOGGER.debug("Did not find any route info in the current rule: {}", rule);
    }

    return null;
  }

  private void restartWaitingTaskTimers() {
    
    threadPool.submit(() -> doRestartWaitingTaskTimers());
  }

  private void doRestartWaitingTaskTimers() {
    LOGGER.debug("Restarting timers for waiting tasks at startup");

    try {
      TaskEnumerableResult enumResults = db.task.enumerableResultFilteredByWaitingState();
      if (enumResults != null) {
        while (enumResults.next()) {
          List<Task> tasks = enumResults.get();
          tasks.forEach(this::attachExpirationTimerToTask);
        }
      }
    } catch (Exception ex) {
      LOGGER.error("Can not restart timers for all waiting tasks!{}", ex.getMessage());
      throw new RuntimeException("Can not restart timers for all waiting tasks!", ex);
    }
  }

  private void attachExpirationTimerToTask(Task task) {

    if (task.getExpirationDate() == null) {
      LOGGER.trace("No expiration date, won't attach timer for task: {}", task.getId());
    } else {

      long timeout = task.getExpirationDate().getTime() - System.currentTimeMillis();

      LOGGER.debug("Attaching expiration timer with delay:{}ms for task:{}", 
          timeout, task.getId());
      ScheduledFuture<?> timer = threadPool.schedule(() -> {
        onQueuedTaskTimeout(task.getId());
      }, timeout, TimeUnit.MILLISECONDS);
    }
  }

  public static class Configuration {

    private static final Configuration DEFAULT =
        new Configuration(10, 2, 60, 500);

    public final int threadPoolSize;
    public final int backOffDelay;
    public final int backOffMaxDelay;
    public final int jitter;

    public Configuration(int threadPoolSize, int backOffDelay, int backOffMaxDelay, int jitter) {
      this.threadPoolSize = threadPoolSize;
      this.backOffDelay = backOffDelay;
      this.backOffMaxDelay = backOffMaxDelay;
      this.jitter = jitter;
    }
  }
}
