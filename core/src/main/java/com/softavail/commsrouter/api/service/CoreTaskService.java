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
import com.softavail.commsrouter.api.interfaces.QueuedTaskListener;
import com.softavail.commsrouter.api.interfaces.TaskService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Route;
import com.softavail.commsrouter.domain.Rule;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.Objects;
import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class CoreTaskService extends CoreRouterObjectService<TaskDto, Task>
    implements TaskService, QueuedTaskListener {

  private static final Logger LOGGER = LogManager.getLogger(CoreTaskService.class);

  public CoreTaskService(AppContext app) {
    super(app, app.db.task, app.entityMapper.task);
    app.taskDispatcher.addQueuedTaskListener(this);
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
    app.taskDispatcher.dispatchQueuedTask(createdTaskDto.getId());
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
    app.taskDispatcher.dispatchQueuedTask(createdTaskDto.getId());
    return createdTaskDto;
  }

  @Override
  public void update(UpdateTaskArg updateArg, RouterObjectId objectId) throws CommsRouterException {

    if (updateArg.getState() != TaskState.completed) {
      throw new BadValueException("Expected state: completed");
    }

    String releasedAgentId = app.db.transactionManager.execute((em) -> {
      Task task = app.db.task.get(em, objectId.getId());
      // @todo: check current state and throw if not appropriate and then agent == null would be
      // internal error
      task.setState(TaskState.completed);


      Agent agent = task.getAgent();
      if (agent == null) {
        return null;
      }
      if (agent.getState() != AgentState.busy) {
        return null;
      }
      agent.setState(AgentState.ready);
      return agent.getId();
    });

    if (releasedAgentId != null) {
      app.taskDispatcher.dispatchAgent(releasedAgentId);
    }
    app.taskDispatcher.dispatchQueuedTask(objectId.getId());
  }

  @Override
  public void update(UpdateTaskContext taskContext, RouterObjectId objectId)
      throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Task task = app.db.task.get(em, objectId.getId());
      task.setUserContext(app.entityMapper.attributes.toJpa(taskContext.getUserContext()));
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

      task.setUserContext(app.entityMapper.attributes.toJpa(existingContext));
    });
  }

  private Route gethMatchedRoute(AttributeGroupDto attributesGroup, List<Rule> rules, Long inRuleId,
      Long prevRouteId) {
    for (Rule rule : rules) {
      if (inRuleId != null && rule.getId() < inRuleId) {
        // nothing here - skip rules until find the required one
      } else if (inRuleId != null && Objects.equals(rule.getId(), inRuleId)) {
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
        // current logic: stop try to find the next expresion matched rule in the plan
        return null;
      } else if (!rule.getRoutes().isEmpty()) {
        try {
          if (app.evaluator.evaluatePredicateByAttributes(attributesGroup, rule.getPredicate())) {
            return rule.getRoutes().get(0);
          }
        } catch (CommsRouterException ex) {
          LOGGER.warn("Evaluation for Queue with ID={} failed : {}",
              rule.getRoutes().get(0).getQueueId(), ex.getLocalizedMessage());
        }
      } else {
        LOGGER.debug("Did not found any route info in the current rule: {}", rule);
      }
    }

    return null;
  }

  private CreatedTaskDto doCreate(EntityManager em, CreateTaskArg createArg,
      RouterObjectId objectId) throws CommsRouterException {

    app.db.router.get(em, objectId.getRouterId());

    Task task = fromPlan(em, createArg, objectId);
    task.setState(TaskState.waiting);
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
      Route matchedRoute =
          gethMatchedRoute(createArg.getRequirements(), plan.getRules(), null, null);
      if (matchedRoute == null) {
        matchedRoute = plan.getDefaultRoute();
      }

      if (matchedRoute == null) {
        throw new NotFoundException("Did not found any Route for task '{}'" + createArg);
      }

      if (matchedRoute.getQueueId() == null) {
        throw new NotFoundException(
            "Evaluator didn't match task to any queues using the plan rules.");
      }

      queueId = matchedRoute.getQueueId();
      task.setPriority(matchedRoute.getPriority());
      task.setQueuedTimeout(matchedRoute.getTimeout());
      task.setPlan(plan);
      task.setRoute(matchedRoute);
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

  @Override
  public void onTaskAddedToQueue(TaskDto task) {
    // ToDo: later for statistics impl.
  }

  @Override
  public void onTaskAssignedToAgent(TaskDto task) {
    // ToDo: later for statistics impl.
  }

  @Override
  public void onQueuedTaskTimeout(TaskDto task) {
    TaskDto taskDto = null;
    try {
      taskDto = app.db.transactionManager.execute((em) -> {
        Task newTask = app.db.task.get(em, task.getId());
        if (null == newTask.getState()) {
          return null;
        }

        switch (newTask.getState()) {
          case completed:
            return null;
          case assigned: {
            if (true) {
              // current tasks timeout logic is used for waiting tasks only!
              return null;
            }
            Agent agent = newTask.getAgent();
            if (agent != null) {
              if (agent.getState() == AgentState.busy) {
                Fields.update(agent::setState, agent.getState(), AgentState.offline);
              }
            }
            newTask.setState(TaskState.waiting);
            Fields.update(newTask::setState, newTask.getState(), TaskState.waiting);
            Fields.update(newTask::setAgent, newTask.getAgent(), null);
          }
            break;
          case waiting: {
            Plan plan = newTask.getPlan();
            if (plan == null) {
              return null;
            }
            if (plan.getDefaultRoute().getId().equals(task.getRouteId())) {
              // default route
              return null;
            }

            Route matchedRoute = gethMatchedRoute(task.getRequirements(), plan.getRules(),
                newTask.getRoute().getRule().getId(), newTask.getRoute().getId());
            if (matchedRoute == null) {
              // if not found any other routes in the current rule - don't
              // switch to default plan route for current logic.
              return null;
              // matchedRoute = plan.getDefaultRoute();
            }

            Fields.update(newTask::setRoute, newTask.getRoute(), matchedRoute);
            if (matchedRoute.getPriority() != null) {
              Fields.update(newTask::setPriority, newTask.getPriority(),
                  matchedRoute.getPriority());
            }
            if (matchedRoute.getTimeout() != null) {
              Fields.update(newTask::setQueuedTimeout, newTask.getQueuedTimeout(),
                  matchedRoute.getTimeout());
            }
            if (matchedRoute.getQueueId() != null) {
              Queue queue = app.db.queue.get(em, RouterObjectId.builder()
                  .setId(matchedRoute.getQueueId()).setRouterId(newTask.getRouterId()).build());
              Fields.update(newTask::setQueue, newTask.getQueue(), queue);
              if (!task.getQueueId().equals(matchedRoute.getQueueId())) {
                Fields.update(newTask::setAgent, newTask.getAgent(), null);
              }
            }
          }
            break;
          default:
            return null;
        }
        app.taskDispatcher.dispatchTask(newTask.getId());
        return entityMapper.toDto(newTask);
      });
    } catch (CommsRouterException ex) {
      LOGGER.warn(ex.getLocalizedMessage());
    }

    if (taskDto != null) {
      app.taskDispatcher.dispatchQueuedTask(taskDto.getId());
    }
  }

  @Override
  public void onQueuedTaskCompleted(TaskDto task) {
    // ToDo: later for statistics impl.
  }

}
