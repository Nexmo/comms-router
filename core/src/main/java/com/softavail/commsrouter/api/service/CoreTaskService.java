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
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.TaskService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Queue;
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
        RouterObjectId.builder().setId(Uuid.get()).setRouterId(routerId).build();

    CreatedTaskDto createdTaskDto = app.db.transactionManager.execute((EntityManager em) -> {
      return doCreate(em, createArg, routerObjectId);
    });

    app.taskDispatcher.dispatchTask(createdTaskDto.getId());
    return createdTaskDto;
  }

  @Override
  public CreatedTaskDto create(CreateTaskArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    validate(createArg);

    CreatedTaskDto createdTaskDto = app.db.transactionManager.execute((em) -> {
      app.db.task.delete(em, objectId.getId());
      return doCreate(em, createArg, objectId);
    });

    app.taskDispatcher.dispatchTask(createdTaskDto.getId());
    return createdTaskDto;
  }

  @Override
  public void update(UpdateTaskArg updateArg, RouterObjectId objectId)
      throws CommsRouterException {

    switch (updateArg.getState()) {
      case waiting:
        app.db.transactionManager
            .execute(em -> app.taskDispatcher.rejectAssignment(em, objectId.getId()))
            .ifPresent(app.taskDispatcher::dispatchTask);
        break;
      case completed:
        app.db.transactionManager
            .execute((em) -> completeTask(em, objectId.getId()))
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
      task.setUserContext(app.entityMapper.attributes.toJpa(taskContext.getUserContext()));
    });
  }

  private Optional<String> completeTask(EntityManager em, String taskId)
      throws NotFoundException {

    Task task = app.db.task.get(em, taskId);
    // @todo: check current state and throw if not appropriate and then agent == null would be
    // internal error
    task.setState(TaskState.completed);
    Agent agent = task.getAgent();
    if (agent == null) {
      return Optional.empty();
    }
    if (agent.getState() != AgentState.busy) {
      return Optional.empty();
    }
    agent.setState(AgentState.ready);
    return Optional.of(agent.getId());
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

      task.setUserContext(app.entityMapper.attributes.toJpa(existingContext));
    });
  }

  private CreatedTaskDto doCreate(EntityManager em, CreateTaskArg createArg,
      RouterObjectId objectId) throws CommsRouterException {

    app.db.router.get(em, objectId.getRouterId());

    Task task = fromPlan(em, createArg, objectId);
    task.setState(TaskState.waiting);
    task.setPriority(createArg.getPriority());
    task.setCallbackUrl(createArg.getCallbackUrl().toString());
    task.setRequirements(app.entityMapper.attributes.toJpa(createArg.getRequirements()));
    task.setUserContext(app.entityMapper.attributes.toJpa(createArg.getUserContext()));

    em.persist(task);

    long queueTasks = app.db.queue.getQueueSize(em, task.getQueue().getId()) - 1;

    TaskDto taskDto = entityMapper.toDto(task);

    return new CreatedTaskDto(taskDto, queueTasks);
  }

  private Task fromPlan(EntityManager em, CreateTaskArg createArg, RouterObjectId objectId)
      throws NotFoundException {

    Task task = new Task(objectId);
    String queueId = createArg.getQueueId();
    if (createArg.getPlanId() != null) {
      Plan plan = app.db.plan.get(em, RouterObjectId.builder().setId(createArg.getPlanId())
          .setRouterId(objectId.getRouterId()).build());
      List<Rule> rules = plan.getRules();
      for (Rule rule : rules) {
        try {
          if (app.evaluator.evaluateNewTaskToQueueByPlanRules(objectId.getId(), createArg, rule)) {
            queueId = rule.getQueueId();
            break;
          }
        } catch (CommsRouterException ex) {
          LOGGER.warn("Evaluation for Queue with ID={} failed : {}", rule.getQueueId(),
              ex.getLocalizedMessage());
        }
      }

      if (queueId == null) {
        throw new IllegalArgumentException(
            "Evaluator didn't match task to any queues using the plan rules.");
      }

      task.setPlan(plan);
    }

    Queue queue = app.db.queue.get(em,
        RouterObjectId.builder().setId(queueId).setRouterId(objectId.getRouterId()).build());
    task.setQueue(queue);

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

}
