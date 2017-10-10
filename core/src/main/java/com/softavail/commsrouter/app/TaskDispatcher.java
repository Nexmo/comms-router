/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.app;

import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.QueuedTaskListener;
import com.softavail.commsrouter.api.interfaces.TaskEventHandler;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import javax.persistence.EntityManager;
import javax.persistence.LockModeType;

/**
 * @author ikrustev
 */
public class TaskDispatcher {

  private static final Logger LOGGER = LogManager.getLogger(TaskDispatcher.class);

  private final JpaDbFacade db;
  private final EntityMappers mappers;
  private final TaskEventHandler taskEventHandler;
  private final ScheduledThreadPoolExecutor threadPool;
  private final List<QueuedTaskListener> queuedTaskListeners;
  private static Map<String, ScheduledFuture<?>> scheduledWaitTasksTimers = new HashMap<>();


  public TaskDispatcher(JpaDbFacade dbFacade, TaskEventHandler taskEventHandler,
      EntityMappers dtoMappers) {
    this.db = dbFacade;
    this.mappers = dtoMappers;
    this.taskEventHandler = taskEventHandler;
    // @todo: config threads count
    this.threadPool = new ScheduledThreadPoolExecutor(10);
    threadPool.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
    queuedTaskListeners = new ArrayList<>();
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

  public void dispatchTask(String taskId) {
    threadPool.submit(() -> {
      try {
        doDispatchTask(taskId);
      } catch (CommsRouterException ex) {
        LOGGER.error("Dispatch task {}: failure: {}", taskId, ex, ex);
      }
    });
  }

  public void dispatchQueuedTask(String taskId) {
    threadPool.submit(() -> {
      doDispatchQueuedTask(taskId);
    });
  }

  public void doDispatchQueuedTask(String taskId) {
    if (taskId == null) {
      return;
    }
    TaskDto taskDto = null;
    try {
      taskDto = db.transactionManager.execute((em) -> {
        Task task = db.task.get(em, taskId);
        return mappers.task.toDto(task);
      });
    } catch (CommsRouterException ex) {
      LOGGER.debug("doDispatchQueuedTask(): Task with ID='{}' not found. Error: {}", taskId,
          ex.getLocalizedMessage());
      return;
    }
    if (taskDto == null) {
      LOGGER.debug("doDispatchQueuedTask(): Task with ID='{}' not found.", taskId);
      return;
    }
    switch (taskDto.getState()) {
      case waiting:
        onTaskAddedToQueue(taskDto);
        break;
      case assigned:
        onTaskAssignedToAgent(taskDto);
        break;
      case completed:
        onQueuedTaskCompleted(taskDto);
        break;
      default:
        LOGGER.error("Unexpected Task state: '{}'", taskDto.getState());
        break;
    }
  }

  public void dispatchAgent(String agentId) {
    threadPool.submit(() -> {
      try {
        doDispatchAgent(agentId);
      } catch (CommsRouterException ex) {
        LOGGER.error("Dispatch agent: {}: failure: {}", agentId, ex, ex);
      }
    });
  }

  @SuppressWarnings("unchecked")
  private void doDispatchTask(String taskId) throws CommsRouterException {

    TaskAssignmentDto taskAssignment =
        db.transactionManager.executeWithLockRetry((EntityManager em) -> {

          String qlString = "SELECT t, a FROM Task t "
              + "JOIN t.queue q JOIN q.agents a WHERE t.id = :taskId and a.state = :agentState"
              + " ORDER BY t.priority DESC";

          List<Object[]> result = em.createQuery(qlString).setParameter("taskId", taskId)
              .setParameter("agentState", AgentState.ready).setMaxResults(1).getResultList();

          if (result.isEmpty()) {
            LOGGER.info("Dispatch task {}: no suitable agent", taskId);
            return null;
          }

          Task task = (Task) result.get(0)[0];

          if (task.getState() != TaskState.waiting) {
            LOGGER.info("Dispatch task {}: task already taken", taskId);
            return null;
          }

          Agent agent = (Agent) result.get(0)[1];

          em.lock(task, LockModeType.OPTIMISTIC);
          em.lock(agent, LockModeType.OPTIMISTIC);

          assignTask(task, agent);

          return new TaskAssignmentDto(mappers.task.toDto(task), mappers.agent.toDto(agent));
        });

    if (taskAssignment != null) {
      LOGGER.info("Dispatch task {}: task {} assgined to agent {}", taskId,
          taskAssignment.getTask(), taskAssignment.getAgent());
      doDispatchQueuedTask(taskAssignment.getTask().getId());
      taskEventHandler.onTaskAssigned(taskAssignment);
    } else {
      LOGGER.info("Dispatch task {}: miss", taskId);
    }
  }

  @SuppressWarnings("unchecked")
  private void doDispatchAgent(String agentId) throws CommsRouterException {

    TaskAssignmentDto taskAssignment = db.transactionManager.executeWithLockRetry((em) -> {

      String qlString = "SELECT t, a FROM Task t " + "JOIN t.queue q JOIN q.agents a "
          + "WHERE a.id = :agentId and a.state = :agentState and t.state = :taskState "
          + "ORDER BY t.priority DESC";

      List<Object[]> result = em.createQuery(qlString).setParameter("agentId", agentId)
          .setParameter("agentState", AgentState.ready).setParameter("taskState", TaskState.waiting)
          .setMaxResults(1).getResultList();

      if (result.isEmpty()) {
        return null;
      }

      Task task = (Task) result.get(0)[0];
      Agent agent = (Agent) result.get(0)[1];

      em.lock(task, LockModeType.OPTIMISTIC);
      em.lock(agent, LockModeType.OPTIMISTIC);

      assignTask(task, agent);
      return new TaskAssignmentDto(mappers.task.toDto(task), mappers.agent.toDto(agent));
    });

    if (taskAssignment != null) {
      LOGGER.info("Dispatch agent {}: task {} assgined to agent {}", agentId,
          taskAssignment.getTask(), taskAssignment.getAgent());
      doDispatchQueuedTask(taskAssignment.getTask().getId());
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

  public void onTaskAddedToQueue(TaskDto taskDto) {

    LOGGER.debug("onTaskAddedToQueue(): Task with ID='{}' added to queue='{}'", taskDto.getId(),
        taskDto.getQueueId());
    queuedTaskListeners.forEach((queuedTaskListener) -> {
      queuedTaskListener.onTaskAddedToQueue(taskDto);
    });

    ScheduledFuture<?> timer = threadPool.schedule(() -> {
      onQueuedTaskTimeout(taskDto);
    }, taskDto.getQueuedTimeout(), TimeUnit.SECONDS);

    scheduledWaitTasksTimers.put(taskDto.getId(), timer);
  }

  public void onTaskAssignedToAgent(TaskDto taskDto) {

    LOGGER.debug("onTaskAssignedToAgent(): Task with ID='{}' assigned to agent='{}'",
        taskDto.getId(), taskDto.getAgentId());
    if (scheduledWaitTasksTimers.containsKey(taskDto.getId())) {
      scheduledWaitTasksTimers.get(taskDto.getId()).cancel(true);
      scheduledWaitTasksTimers.remove(taskDto.getId());
    }
    queuedTaskListeners.forEach((queuedTaskListener) -> {
      queuedTaskListener.onTaskAssignedToAgent(taskDto);
    });
  }

  public void onQueuedTaskTimeout(TaskDto taskDto) {

    LOGGER.debug("onQueuedTaskTimeout(): Task with ID='{}' timed-out", taskDto.getId());
    if (scheduledWaitTasksTimers.containsKey(taskDto.getId())) {
      scheduledWaitTasksTimers.remove(taskDto.getId());
    }
    queuedTaskListeners.forEach((queuedTaskListener) -> {
      queuedTaskListener.onQueuedTaskTimeout(taskDto);
    });
  }

  public void onQueuedTaskCompleted(TaskDto taskDto) {

    LOGGER.debug("onQueuedTaskCompleted(): Task with ID='{}' timed-out", taskDto.getId());
    if (scheduledWaitTasksTimers.containsKey(taskDto.getId())) {
      scheduledWaitTasksTimers.get(taskDto.getId()).cancel(true);
      scheduledWaitTasksTimers.remove(taskDto.getId());
    }
    queuedTaskListeners.forEach((queuedTaskListener) -> {
      queuedTaskListener.onQueuedTaskCompleted(taskDto);
    });
  }

  public boolean addQueuedTaskListener(QueuedTaskListener queuedTaskListener) {
    return queuedTaskListeners.add(queuedTaskListener);
  }

  public boolean removeQueuedTaskListener(QueuedTaskListener queuedTaskListener) {
    return queuedTaskListeners.remove(queuedTaskListener);
  }

}
