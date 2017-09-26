/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.TaskService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Task;

import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class CoreTaskService extends CoreRouterObjectService<TaskDto, Task> implements TaskService {

  public CoreTaskService(AppContext app) {
    super(app, app.db.task, app.entityMapper.task);
  }

  @Override
  public TaskDto create(CreateTaskArg createArg) throws CommsRouterException {

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

    TaskDto taskDto = app.db.transactionManager.execute((EntityManager em) -> {
      app.db.router.get(em, createArg.getRouterId());

      if (createArg.getPlanId() != null) {
        Plan plan = app.db.plan.get(em, RouterObject.builder().setId(createArg.getPlanId())
            .setRouterId(createArg.getRouterId()).build());
        String queueId = app.evaluator.evaluateNewTaskToQueueByPlanRules(createArg, plan);
        if (queueId == null) {
          throw new IllegalArgumentException(
              "Evaluator didn't match task to any queues using the plan rules.");
        }
      }

      Task task = new Task(ensureIdPresent(createArg));
      task.setState(TaskState.waiting);

      Queue queue = app.db.queue.get(em, RouterObject.builder().setId(createArg.getQueueId())
          .setRouterId(createArg.getRouterId()).build());

      task.setQueue(queue);
      task.setCallbackUrl(createArg.getCallbackUrl().toString());
      task.setRequirements(app.entityMapper.attributes.toJpa(createArg.getRequirements()));
      task.setUserContext(app.entityMapper.attributes.toJpa(createArg.getUserContext()));

      em.persist(task);
      return entityMapper.toDto(task);
    });

    app.taskDispatcher.dispatchTask(taskDto.getId());
    return taskDto;
  }

  @Override
  public void update(UpdateTaskArg updateArg) throws CommsRouterException {

    if (updateArg.getState() != TaskState.completed) {
      throw new BadValueException("Expected state: completed");
    }

    app.db.transactionManager.executeVoid((em) -> {
      Task task = app.db.task.get(em, updateArg);
      // @todo: check current state and throw if not appropriate and then agent == null would be
      // internal error
      task.setState(TaskState.completed);
      Agent agent = task.getAgent();
      if (agent != null) {
        if (agent.getState() == AgentState.busy) {
          agent.setState(AgentState.ready);
          app.taskDispatcher.dispatchAgent(agent.getId());
        }
      }
    });
  }

  @Override
  public void update(UpdateTaskContext taskContext) throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Task task = app.db.task.get(em, taskContext);
      task.setUserContext(app.entityMapper.attributes.toJpa(taskContext.getUserContext()));
    });
  }

}
