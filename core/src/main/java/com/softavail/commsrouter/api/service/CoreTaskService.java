/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.InvalidStateException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.TaskService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Route;
import com.softavail.commsrouter.domain.Rule;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.util.Uuid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.Optional;
import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class CoreTaskService extends CoreRouterObjectService<TaskDto, Task> implements TaskService {

  private static final Logger LOGGER = LogManager.getLogger(CoreTaskService.class);

  public CoreTaskService(AppContext app) {
    super(app, app.db.task, app.entityMapper.task);
  }

  @Override
  public CreatedTaskDto create(CreateTaskArg createArg, String routerId)
      throws CommsRouterException {

    validate(createArg);

    RouterObjectId routerObjectId =
        RouterObjectId.builder()
            .setId(Uuid.get())
            .setRouterId(routerId)
            .build();

    CreateTaskResult createTaskResult = app.db.transactionManager
        .execute(em -> doCreate(em, createArg, routerObjectId));

    app.taskDispatcher.dispatchTask(createTaskResult.getTaskDto());

    return new CreatedTaskDto(
        createTaskResult.getTaskDto(),
        createTaskResult.getTaskDto().getQueueId(),
        createTaskResult.getQueuePosition());
  }

  @Override
  public CreatedTaskDto create(CreateTaskArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    validate(createArg);

    CreateTaskResult createTaskResult = app.db.transactionManager.execute(em -> {
      Task task = em.find(Task.class, objectId.getId());
      if (task != null) {
        if (!task.getState().isDeleteAllowed()) {
          throw new InvalidStateException(
              "Replacing task in state " + task.getState() + " not allowed");
        }
        em.remove(task);
      }
      return doCreate(em, createArg, objectId);
    });

    app.taskDispatcher.dispatchTask(createTaskResult.getTaskDto());

    return new CreatedTaskDto(
        createTaskResult.getTaskDto(),
        createTaskResult.getTaskDto().getQueueId(),
        createTaskResult.getQueuePosition());
  }

  @Override
  public void update(UpdateTaskArg updateArg, RouterObjectId objectId)
      throws CommsRouterException {

    switch (updateArg.getState()) {
      case waiting:
        app.db.transactionManager
            .execute(em -> rejectAssignment(em, objectId.getId()))
            .ifPresent(app.taskDispatcher::dispatchTask);
        break;
      case completed:
        app.db.transactionManager
            .execute(em -> completeTask(em, objectId.getId()))
            .ifPresent(app.taskDispatcher::dispatchAgent);
        break;
      case assigned:
      default:
        throw new BadValueException("Expected state: waiting or completed");
    }
  }

  @Override
  public void update(UpdateTaskContext taskContext, RouterObjectId objectId)
      throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Task task = app.db.task.get(em, objectId.getId());
      task.setUserContext(app.entityMapper.attributes.fromDto(taskContext.getUserContext()));
    });
  }

  @Override
  public void updateContext(UpdateTaskContext taskContext, RouterObjectId objectId)
      throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Task task = app.db.task.get(em, objectId.getId());
      AttributeGroupDto existingContext = app.entityMapper.attributes.toDto(task.getUserContext());
      AttributeGroupDto newContext = taskContext.getUserContext();

      if (null == existingContext) {
        existingContext = newContext;
      } else {
        for (String key : newContext.keySet()) {
          existingContext.put(key, newContext.get(key));
        }
      }

      task.setUserContext(app.entityMapper.attributes.fromDto(existingContext));
    });
  }

  private Route getMatchedRoute(String taskId, AttributeGroupDto attributesGroup, Rule rule) {
    if (rule != null) {
      if (rule.getRoutes().isEmpty()) {
        return null;
      }

      try {
        if (app.evaluator.evaluate(attributesGroup, rule.getPredicate())) {
          LOGGER.info("Task {}: matched rule {} tag {}", taskId, rule.getId(), rule.getTag());
          return rule.getRoutes().get(0);
        }
      } catch (CommsRouterException ex) {
        LOGGER.error("Task {}: failure matching rule {} tag {}: {}", taskId, rule.getId(),
            rule.getTag(), ex, ex);
      }

      LOGGER.debug("Did not found any route info in the current rule: {}", rule);
    }

    return null;
  }

  private static class CreateTaskResult {
    private TaskDto taskDto;
    private long queuePosition;

    public TaskDto getTaskDto() {
      return taskDto;
    }

    public void setTaskDto(TaskDto taskDto) {
      this.taskDto = taskDto;
    }

    public long getQueuePosition() {
      return queuePosition;
    }

    public void setQueuePosition(long queuePosition) {
      this.queuePosition = queuePosition;
    }
  }

  private CreateTaskResult doCreate(EntityManager em, CreateTaskArg createArg, RouterObjectId obj)
      throws CommsRouterException {

    app.db.router.get(em, obj.getRouterId());

    Task task = fromPlan(em, createArg, obj);
    task.setState(TaskState.waiting);
    task.setCallbackUrl(createArg.getCallbackUrl().toString());
    task.setRequirements(app.entityMapper.attributes.fromDto(createArg.getRequirements()));
    task.setUserContext(app.entityMapper.attributes.fromDto(createArg.getUserContext()));

    em.persist(task);

    String queueId = task.getQueue().getId();

    CreateTaskResult result = new CreateTaskResult();

    result.setQueuePosition(app.db.queue.getQueueSize(em, queueId) - 1);
    result.setTaskDto(entityMapper.toDto(task));
    return result;
  }

  private Task fromPlan(EntityManager em, CreateTaskArg createArg, RouterObjectId objectId)
      throws NotFoundException {

    Task task = new Task(objectId);

    if (createArg.getPlanId() != null) {

      Plan plan = app.db.plan.get(em, RouterObjectId.builder().setId(createArg.getPlanId())
          .setRouterId(objectId.getRouterId()).build());
      Route matchedRoute = null;
      List<Rule> rules = plan.getRules();
      for (Rule rule : rules) {
        matchedRoute = getMatchedRoute(task.getId(), createArg.getRequirements(), rule);
        if (matchedRoute != null) {
          task.setRule(rule);
          break;
        }
      }

      if (matchedRoute == null) {
        matchedRoute = plan.getDefaultRoute();
      }

      if (matchedRoute == null) {
        throw new NotFoundException("Did not found any Route for task '{}'" + createArg);
      }

      if (matchedRoute.getQueue() == null) {
        throw new NotFoundException(
            "Evaluator didn't match task to any queues using the plan rules.");
      }

      task.setQueue(matchedRoute.getQueue());
      task.setPriority(matchedRoute.getPriority());
      task.setQueuedTimeout(matchedRoute.getTimeout());
      task.setCurrentRoute(matchedRoute);

    } else {

      Queue queue = app.db.queue.get(em, RouterObjectId.builder().setId(createArg.getQueueId())
          .setRouterId(objectId.getRouterId()).build());
      task.setQueue(queue);
    }

    return task;
  }

  private void validate(CreateTaskArg createArg) {

    // TODO Do it with javax.validation?!

    if (createArg.getCallbackUrl() == null) {
      throw new IllegalArgumentException("callbackUrl is required");
    }

    if (createArg.getPlanId() == null && createArg.getQueueId() == null) {
      throw new IllegalArgumentException(
          "Missing required argument: please provide either planId or queueId");
    }

    if (createArg.getPlanId() != null && createArg.getQueueId() != null) {
      throw new IllegalArgumentException("Provide either planId or queueId, but not both");
    }

  }

  private Optional<TaskDto> rejectAssignment(EntityManager em, String taskId)
      throws NotFoundException {

    Task task = app.db.task.get(em, taskId);
    Agent agent = task.getAgent();

    if (task.getState().isAssigned() && agent != null) {
      agent.setState(AgentState.unavailable);
      task.setState(TaskState.waiting);
      task.setAgent(null);

      return Optional.of(app.entityMapper.task.toDto(task));
    }

    return Optional.empty();
  }

  private Optional<String> completeTask(EntityManager em, String taskId)
      throws NotFoundException, InvalidStateException {

    Task task = app.db.task.get(em, taskId);

    switch (task.getState()) {
      case completed:
        throw new InvalidStateException("Task already completed");
      case waiting:
        assert task.getAgent() == null : "Waiting task " + task.getId() + " has assigned agent: "
            + task.getAgent().getId();
        task.makeCompleted();
        return Optional.empty();
      case assigned:
        break;
      default:
        throw new InvalidStateException(
            "Current state cannot be switched to completed: " + task.getState());
    }

    Agent agent = task.getAgent();

    assert agent != null : "Completed task with no agent: " + task.getId();

    task.makeCompleted();

    if (agent.getState() != AgentState.busy) {
      assert false
          : "Invalid agent state '" + agent.getState() + "' for completed task: " + task.getId();
      return Optional.empty();
    }
    agent.setState(AgentState.ready);
    return Optional.of(agent.getId());
  }

  @Override
  public void delete(RouterObjectId routerObjectId) throws CommsRouterException {
    app.db.transactionManager.executeVoid((em) -> {
      doDelete(em, routerObjectId);
    });
  }

  private void doDelete(EntityManager em, RouterObjectId routerObjectId)
      throws NotFoundException, InvalidStateException {

    Task task = app.db.task.get(em, routerObjectId);
    if (!task.getState().isDeleteAllowed()) {
      throw new InvalidStateException("Deleting task in state " + task.getState() + " not allowed");
    }
    em.remove(task);
  }

}
