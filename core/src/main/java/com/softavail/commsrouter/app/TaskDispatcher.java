/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.app;

import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.TaskEventHandler;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.ApiObject;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Route;
import com.softavail.commsrouter.domain.Rule;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import com.softavail.commsrouter.util.Fields;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class TaskDispatcher {

  private static final Logger LOGGER = LogManager.getLogger(TaskDispatcher.class);

  private final JpaDbFacade db;
  private final EntityMappers mappers;
  private final TaskEventHandler taskEventHandler;
  private final ScheduledThreadPoolExecutor threadPool;
  private final QueueProcessorManager queueProcessorManager;
  private static Map<String, ScheduledFuture<?>> scheduledWaitTasksTimers = new HashMap<>();

  public TaskDispatcher(JpaDbFacade db, TaskEventHandler taskEventHandler, EntityMappers dtoMappers,
      int threadPoolSize) {

    this.db = db;
    this.mappers = dtoMappers;
    this.taskEventHandler = taskEventHandler;
    this.threadPool = new ScheduledThreadPoolExecutor(threadPoolSize);
    this.threadPool.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
    this.queueProcessorManager = QueueProcessorManager.getInstance();
    startQueueProcessors();
  }

  @SuppressWarnings("unchecked")
  private void startQueueProcessors() {
    try {
      db.transactionManager.executeVoid(
          em -> db.router.list(em).stream().map(router -> db.queue.list(em, router.getId()))
              .flatMap(Collection::stream).map(Queue::getId).forEach(this::process));
    } catch (CommsRouterException e) {
      throw new RuntimeException("Can not instantiate TaskDispatcher!", e);
    }
  }

  private void process(String queueId) {
    queueProcessorManager.processQueue(queueId, db, mappers, this, taskEventHandler, threadPool);
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

  public void dispatchTask(TaskDto taskDto) {
    process(taskDto.getQueueId());
    startTaskTimer(taskDto);
  }

  public void dispatchAgent(String agentId) {
    try {
      // Get the queueId from the agent
      db.transactionManager.executeVoid(em -> db.agent.get(em, agentId).getQueues().parallelStream()
          .map(ApiObject::getId).forEach(this::process));
    } catch (RuntimeException | CommsRouterException e) {
      LOGGER.error("Dispatch task {}: failure: {}", agentId, e, e);
    }
  }

  public Optional<TaskDto> rejectAssignment(EntityManager em, String taskId)
      throws NotFoundException {

    Task task = db.task.get(em, taskId);
    Agent agent = task.getAgent();

    if (task.getState().isAssigned() && agent != null) {
      agent.setState(AgentState.unavailable);
      task.setState(TaskState.waiting);
      task.setAgent(null);

      return Optional.of(mappers.task.toDto(task));
    }

    return Optional.empty();
  }

  public void rejectAssignment(String taskId) {
    LOGGER.debug("Rejecting assignment of task {}", taskId);
    try {
      db.transactionManager.execute(em -> rejectAssignment(em, taskId))
          .ifPresent(this::dispatchTask);
    } catch (CommsRouterException | RuntimeException ex) {
      LOGGER.error("Failure rejecting assignment: {}", ex, ex);
    }
  }

  private void startTaskTimer(TaskDto taskDto) {

    LOGGER.debug("Starting wait timer for task {}", taskDto.getId());

    ScheduledFuture<?> timer = threadPool.schedule(() -> {
      onQueuedTaskTimeout(taskDto);
    }, taskDto.getQueuedTimeout(), TimeUnit.SECONDS);

    scheduledWaitTasksTimers.put(taskDto.getId(), timer);
  }

  public void onQueuedTaskTimeout(TaskDto taskDto) {

    try {
      LOGGER.debug("onQueuedTaskTimeout(): Task with ID='{}' timed-out", taskDto.getId());
      scheduledWaitTasksTimers.remove(taskDto.getId());
      processTaskTimeout(taskDto.getId());
    } catch (RuntimeException | CommsRouterException ex) {
      LOGGER.error("Exception while provessing timeout for task {}: {}", taskDto.getId(), ex, ex);
    }
  }

  private void processTaskTimeout(String taskId) throws CommsRouterException {

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
            return null;
          }

          if (matchedRoute == null) {
            return null;
          }

          Fields.update(task::setCurrentRoute, task.getCurrentRoute(), matchedRoute);
          if (matchedRoute.getPriority() != null) {
            Fields.update(task::setPriority, task.getPriority(), matchedRoute.getPriority());
          }
          if (matchedRoute.getTimeout() != null) {
            Fields.update(task::setQueuedTimeout, task.getQueuedTimeout(),
                matchedRoute.getTimeout());
          }
          if (matchedRoute.getQueueId() != null) {
            Queue queue = db.queue.get(em, RouterObjectId.builder().setId(matchedRoute.getQueueId())
                .setRouterId(task.getRouterId()).build());
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

}
