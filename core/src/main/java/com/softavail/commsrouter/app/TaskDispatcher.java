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

import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CallbackException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.TaskEventHandler;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Route;
import com.softavail.commsrouter.domain.Rule;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.domain.result.MatchResult;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import com.softavail.commsrouter.jpa.result.TaskEnumerableResult;
import com.softavail.commsrouter.util.ThreadPoolKiller;
import net.jodah.failsafe.Failsafe;
import net.jodah.failsafe.RetryPolicy;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author ikrustev
 */
public class TaskDispatcher {

  private static final Logger LOGGER = LogManager.getLogger(TaskDispatcher.class);

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
    Integer backoffDelay = configuration.getBackoffDelay();
    Integer backoffDelayMax = configuration.getBackoffDelayMax();
    this.retryPolicy = new RetryPolicy()
        .retryOn(CallbackException.class)
        .retryOn(RuntimeException.class)
        .withBackoff(backoffDelay, backoffDelayMax, TimeUnit.SECONDS)
        .withJitter(configuration.getJitter(), TimeUnit.MILLISECONDS);
    startQueueProcessors();
    restartWaitingTaskTimers();
  }

  @SuppressWarnings("unchecked")
  private void startQueueProcessors() {
    try {
      db.transactionManager.executeVoid(em ->
          db.router.list(em).forEach(router -> {
            db.queue.list(em, router.getRef()).forEach(queue -> {
              process(router.getId(), queue.getId());
            });
          })
      );
    } catch (CommsRouterException e) {
      throw new RuntimeException("Can not instantiate TaskDispatcher!", e);
    }
  }

  private void process(Long routerId, Long queueId) {
    queueProcessorManager
        .processQueue(routerId, queueId, db, mappers, this, configuration, threadPool);
  }

  public void close() {
    final Integer shutdownDelay = configuration.getDispatcherThreadShutdownDelay();
    ThreadPoolKiller.shutdown(threadPool, "TaskDispatcher", shutdownDelay);
  }

  public void dispatchTask(TaskDispatchInfo dispatchInfo) {
    process(dispatchInfo.getRouterId(), dispatchInfo.getQueueId());
    setTaskExpirationTimeout(dispatchInfo.getTaskId(), dispatchInfo.getQueuedTimeout());
  }

  public void dispatchAgent(AgentDispatchInfo dispatchInfo) {
    threadPool.submit(() -> {
      try {
        doDispatchAgent(dispatchInfo);
      } catch (RuntimeException | CommsRouterException e) {
        LOGGER.error("Dispatch agent {}: failure: {}", dispatchInfo.getAgentId(), e, e);
      }
    });
  }

  private void doDispatchAgent(AgentDispatchInfo dispatchInfo) throws CommsRouterException {

    TaskAssignmentDto taskAssignmentDto = db.transactionManager.executeWithLockRetry(
        em -> {
          db.router.lock(em, dispatchInfo.getRouterId());

          return db.queue.findAssignmentForAgent(em, dispatchInfo.getAgentId())
              .map(this::assignTask)
              .orElse(null);
        }
    );

    if (taskAssignmentDto != null) {
      submitTaskAssignment(taskAssignmentDto);
    }
  }

  public TaskAssignmentDto assignTask(MatchResult matchResult) {
    Agent agent = matchResult.agent;
    Task task = matchResult.task;
    // Assign
    agent.setState(AgentState.busy);
    task.setState(TaskState.assigned);
    task.setAgent(agent);

    TaskDto taskDto = mappers.task.toDto(task);
    AgentDto agentDto = mappers.agent.toDto(agent);
    return new TaskAssignmentDto(taskDto, agentDto);
  }

  public void submitTaskAssignment(TaskAssignmentDto taskAssignmentDto) {
    RetryPolicy retryPolicy = this.retryPolicy.copy();
    retryPolicy.abortIf(obj -> {
      try {
        return db.transactionManager.execute(em -> {
          Task task = db.task.get(em, taskAssignmentDto.getTask());
          return task.getState() != TaskState.assigned;
        });
      } catch (CommsRouterException e) {
        LOGGER.debug("Error retrieving Task: {}", taskAssignmentDto.getTask().getRef());
        return true;
      }
    });
    Failsafe.with(retryPolicy).with(threadPool)
        .onSuccess((ignored, executionContext) -> LOGGER.debug("Task {} assigned to agent {}",
            taskAssignmentDto.getTask(), taskAssignmentDto.getAgent()))
        .onRetry(
            (result, failure, context) -> LOGGER.warn("Retry assigning task {} to agent {}: {}, {}",
                taskAssignmentDto.getTask(), taskAssignmentDto.getAgent(), failure, context))
        .onFailure((ignored, throwable) -> LOGGER.error("Failure assigning task {} to agent {}: {}",
            taskAssignmentDto.getTask(), taskAssignmentDto.getAgent(), throwable, throwable))
        .run(() -> taskEventHandler.onTaskAssigned(taskAssignmentDto));
  }

  private void setTaskExpirationTimeout(Long taskId, Long seconds) {

    LOGGER.debug("Set expiration timeout:{} for task:{}", seconds, taskId);

    threadPool.schedule(() -> {
      onQueuedTaskTimeout(taskId);
    }, seconds, TimeUnit.SECONDS);
  }

  private void onQueuedTaskTimeout(Long taskId) {

    try {
      LOGGER.debug("onQueuedTaskTimeout(): Task with ID='{}' timed-out", taskId);
      processTaskTimeout(taskId);
    } catch (RuntimeException | CommsRouterException ex) {
      LOGGER.error("Exception while processing timeout for task {}: {}", taskId, ex);
    }
  }

  private void processTaskTimeout(Long taskId) throws CommsRouterException {

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
            task.setExpirationDate(null);
            return null;
          }

          if (matchedRoute == null) {
            task.setExpirationDate(null);
            return null;
          }

          task.setCurrentRoute(matchedRoute);

          if (matchedRoute.getPriority() != null) {
            task.setPriority(matchedRoute.getPriority());
          }

          Date expirationDate = null;
          if (matchedRoute.getTimeout() != null) {
            task.setQueuedTimeout(matchedRoute.getTimeout());

            if (matchedRoute.getTimeout() > 0) {
              expirationDate =
                  new Date(System.currentTimeMillis() + matchedRoute.getTimeout() * 1000);
              LOGGER.trace("Next route, update expirationDate:{} for task:{} ", expirationDate,
                  task.getRef());
            } else {
              LOGGER.trace("Next route, clear expirationDate for task:{}", task.getRef());
            }
          } else if (task.getQueuedTimeout() != null) {
            if (task.getQueuedTimeout() > 0) {
              expirationDate =
                  new Date(System.currentTimeMillis() + task.getQueuedTimeout() * 1000);
              LOGGER.trace("Default, update expirationDate:{} for task:{} ", expirationDate,
                  task.getRef());
            } else {
              LOGGER.trace("Default, clear expirationDate for task:{}", task.getRef());
            }
          } else {
            LOGGER.trace("None, clear expirationDate for task:{}", task.getRef());
          }
          task.setExpirationDate(expirationDate);

          if (matchedRoute.getQueue() != null) {
            task.setQueue(matchedRoute.getQueue());
          }
          break;
        }
        default:
          return null;
      }
      return mappers.task.toDto(task);
    });

    if (taskDto != null) {
      setTaskExpirationTimeout(taskDto.getId(), taskDto.getQueuedTimeout());
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
    threadPool.submit(this::doRestartWaitingTaskTimers);
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
      LOGGER.trace("No expiration date, won't attach timer for task: {}", task.getRef());
    } else {
      long seconds = (task.getExpirationDate().getTime() - System.currentTimeMillis()) / 1000;
      setTaskExpirationTimeout(task.getId(), seconds);
    }
  }

}
