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

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.InternalErrorException;
import com.softavail.commsrouter.api.exception.InvalidStateException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.TaskService;
import com.softavail.commsrouter.app.AgentDispatchInfo;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.app.TaskDispatchInfo;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.AttributeGroup;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Route;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.domain.Rule;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import com.softavail.commsrouter.util.Uuid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

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

    RouterObjectRef routerObjectId =
        RouterObjectRef.builder().setRef(Uuid.get()).setRouterRef(routerId)
            .build();

    TaskDispatchInfo dispatchInfo = app.db.transactionManager
        .execute(em -> doCreate(em, createArg, routerObjectId));

    app.taskDispatcher.dispatchTask(dispatchInfo);

    return new CreatedTaskDto(dispatchInfo.getTaskRef(), dispatchInfo.getQueuePosition());
  }

  @Override
  public TaskDto getByTag(String routerId, String tag)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      Task entity = app.db.task.getByTag(em, routerId, tag);
      return entityMapper.toDto(entity);
    });
  }

  @Override
  public CreatedTaskDto replace(CreateTaskArg createArg, RouterObjectRef objectId)

      throws CommsRouterException {

    validate(createArg);

    TaskDispatchInfo dispatchInfo = app.db.transactionManager.execute(em -> {

      Task task = repository.getNoThrow(em, objectId);
      if (task != null) {
        if (!task.getState().isDeleteAllowed()) {
          throw new InvalidStateException(
              "Replacing task in state " + task.getState() + " not allowed");
        }
        em.remove(task);
        em.flush();
      }
      return doCreate(em, createArg, objectId);
    });

    app.taskDispatcher.dispatchTask(dispatchInfo);

    return new CreatedTaskDto(dispatchInfo.getTaskRef(), dispatchInfo.getQueuePosition());
  }

  @Override
  public void update(UpdateTaskArg updateArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    switch (updateArg.getState()) {
      case waiting:
        rejectTaskAssignment(objectRef);
        break;
      case canceled:
        app.db.transactionManager.executeVoidWithLockRetry(em -> cancelTask(em, objectRef));
        break;
      case completed:
        completeTask(objectRef);
        break;
      case assigned:
      default:
        throw new BadValueException("Expected state: canceled, waiting or completed");
    }
  }

  @Override
  public void update(UpdateTaskContext taskContext, RouterObjectRef objectRef)
      throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Task task = app.db.task.get(em, objectRef);
      task.setUserContext(app.entityMapper.attributes.fromDto(taskContext.getUserContext()));
    });
  }

  @Override
  public void updateContext(UpdateTaskContext taskContext, RouterObjectRef objectRef)
      throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Task task = app.db.task.get(em, objectRef);
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

  private Route getMatchedRoute(
      String taskId, AttributeGroup attributesGroup, Rule rule, CommsRouterEvaluator evaluator)
      throws CommsRouterException {
    if (rule != null) {
      if (rule.getRoutes().isEmpty()) {
        return null;
      }

      try {
        if (evaluator.evaluate(attributesGroup)) {
          LOGGER.info("Task {}: matched rule {} tag {}", taskId, rule.getId(), rule.getTag());
          return rule.getRoutes().get(0);
        }
      } catch (CommsRouterException ex) {
        LOGGER.error("Task {}: failure matching rule {} tag {}: {}", taskId, rule.getId(),
            rule.getTag(), ex, ex);
        throw ex;
      }

      LOGGER.debug("Did not found any route info in the current rule: {}", rule);
    }

    return null;
  }

  private TaskDispatchInfo doCreate(EntityManager em, CreateTaskArg createArg, RouterObjectRef obj)
      throws CommsRouterException {

    // validate requirements
    app.validators.taskRequirementsValidator
        .validate(createArg.getRequirements(), obj.getRouterRef());

    Task task = fromPlan(em, createArg, obj);
    task.setState(TaskState.waiting);
    task.setCallbackUrl(createArg.getCallbackUrl().toString());
    task.setUserContext(app.entityMapper.attributes.fromDto(createArg.getUserContext()));
    task.setTag(createArg.getTag());

    em.persist(task);

    Long queueId = task.getQueue().getId();

    TaskDispatchInfo result = app.entityMapper.task.toDispatchInfo(task);
    result.setQueuePosition(app.db.queue.getQueueSize(em, queueId) - 1);
    return result;
  }

  private Task fromPlan(EntityManager em, CreateTaskArg createArg, RouterObjectRef objectId)
      throws NotFoundException, CommsRouterException {

    Router router = getRouter(em, objectId);
    Task task = new Task(objectId);
    task.setRouter(router);
    task.setRequirements(app.entityMapper.attributes.fromDto(createArg.getRequirements()));

    if (createArg.getPlanRef() != null) {

      Plan plan = app.db.plan.get(em, RouterObjectRef.builder().setRef(createArg.getPlanRef())
          .setRouterRef(objectId.getRouterRef()).build());
      Route matchedRoute = null;
      CommsRouterEvaluator evaluator = app.evaluatorFactory.provide(null, null);
      List<Rule> rules = plan.getRules();
      for (Rule rule : rules) {
        evaluator = evaluator.changeExpression(rule.getPredicate(), objectId.getRouterRef());
        matchedRoute = getMatchedRoute(task.getRef(), task.getRequirements(), rule, evaluator);
        if (matchedRoute != null) {
          task.setRule(rule);
          break;
        }
      }

      if (matchedRoute == null) {
        matchedRoute = plan.getDefaultRoute();
      }

      if (matchedRoute == null) {
        throw new NotFoundException("Route task '{}' not found" + createArg);
      }

      if (matchedRoute.getQueue() == null) {
        throw new NotFoundException(
            "Evaluator didn't match task to any queues using the plan rules.");
      }

      task.setQueue(matchedRoute.getQueue());
      task.setPriority(matchedRoute.getPriority());
      task.setQueuedTimeout(matchedRoute.getTimeout());

      if (task.getQueuedTimeout() > 0) {
        task.setExpirationDate(new Date(
            System.currentTimeMillis() + TimeUnit.SECONDS.toMillis(task.getQueuedTimeout())));
      }

      task.setCurrentRoute(matchedRoute);

    } else {

      Queue queue = app.db.queue.get(em, RouterObjectRef.builder().setRef(createArg.getQueueRef())
          .setRouterRef(objectId.getRouterRef()).build());
      task.setQueue(queue);
    }

    return task;
  }

  private void validate(CreateTaskArg createArg) {

    // TODO Do it with javax.validation?!

    if (createArg.getCallbackUrl() == null) {
      throw new IllegalArgumentException("callbackUrl is required");
    }

    if (createArg.getPlanRef() == null && createArg.getQueueRef() == null) {
      throw new IllegalArgumentException(
          "Missing required argument: please provide either planId or queueId");
    }

    if (createArg.getPlanRef() != null && createArg.getQueueRef() != null) {
      throw new IllegalArgumentException("Provide either planId or queueId, but not both");
    }

  }

  private TaskDispatchInfo rejectAssignment(EntityManager em, RouterObjectRef taskRef)
      throws NotFoundException, InvalidStateException, InternalErrorException {

    Task task = app.db.task.get(em, taskRef);

    switch (task.getState()) {
      case assigned:
        break;
      case completed:
      case canceled:
      case waiting:
      default:
        throw new InvalidStateException(
            "Current state cannot be switched to waiting: " + task.getState());
    }

    Agent agent = task.getAgent();
    assert agent != null : "Rejected task with no agent: " + task.getRef();

    if (agent.getState() != AgentState.busy) {
      throw new InternalErrorException("Unexpected agent state: " + agent.getState());
    }
    agent.setState(AgentState.unavailable);

    task.setState(TaskState.waiting);
    task.setAgent(null);

    return app.entityMapper.task.toDispatchInfo(task);
  }

  private void rejectTaskAssignment(RouterObjectRef objectRef) throws CommsRouterException {
    final TaskDispatchInfo dispatchInfo = app.db.transactionManager
        .executeWithLockRetry(em -> rejectAssignment(em, objectRef));
    app.taskDispatcher.dispatchTask(dispatchInfo);
  }

  private void completeTask(RouterObjectRef objectRef) throws CommsRouterException {
    final AgentDispatchInfo dispatchInfo = app.db.transactionManager
        .executeWithLockRetry(em -> completeTask(em, objectRef));
    app.taskDispatcher.dispatchAgent(dispatchInfo);
  }

  private AgentDispatchInfo completeTask(EntityManager em, RouterObjectRef taskRef)
      throws NotFoundException, InvalidStateException, InternalErrorException {

    Task task = app.db.task.get(em, taskRef);

    switch (task.getState()) {
      case completed:
        throw new InvalidStateException("Task already completed");
      case assigned:
        break;
      case canceled:
      case waiting:
      default:
        throw new InvalidStateException(
            "Current state cannot be switched to completed: " + task.getState());
    }

    Agent agent = task.getAgent();

    assert agent != null : "Completed task with no agent: " + task.getRef();

    task.makeCompleted();

    if (agent.getState() != AgentState.busy) {
      throw new InternalErrorException("Unexpected agent state: " + agent.getState());
    }
    agent.setState(AgentState.ready);
    AgentDispatchInfo dispatchInfo = new AgentDispatchInfo();
    dispatchInfo.setAgentId(agent.getId());
    dispatchInfo.setRouterId(agent.getRouter().getId());
    return dispatchInfo;
  }

  private void cancelTask(EntityManager em, RouterObjectRef taskRef)
      throws NotFoundException, InvalidStateException {

    Task task = app.db.task.get(em, taskRef);

    switch (task.getState()) {
      case waiting:
        assert task.getAgent() == null : "Waiting task " + task.getRef() + " has assigned agent: "
            + task.getAgent().getRef();
        task.makeCanceled();
        return;
      case canceled:
        throw new InvalidStateException("Task already canceled");
      case assigned:
      case completed:
      default:
        throw new InvalidStateException(
            "Current state cannot be switched to canceled: " + task.getState());
    }
  }

  @Override
  public void delete(RouterObjectRef routerObjectRef) throws CommsRouterException {
    app.db.transactionManager.executeVoid((em) -> {
      doDelete(em, routerObjectRef);
    });
  }

  private void doDelete(EntityManager em, RouterObjectRef routerObjectRef)
      throws NotFoundException, InvalidStateException {

    Task task = app.db.task.get(em, routerObjectRef);
    if (!task.getState().isDeleteAllowed()) {
      throw new InvalidStateException("Deleting task in state " + task.getState() + " not allowed");
    }
    em.remove(task);
  }

}
