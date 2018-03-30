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

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExpressionException;
import com.softavail.commsrouter.api.interfaces.QueueService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.AgentQueueMapping;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import javax.persistence.EntityManager;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class CoreQueueService extends CoreRouterObjectService<QueueDto, Queue>
    implements QueueService {

  private static final Logger LOGGER = LogManager.getLogger(CoreQueueService.class);

  public CoreQueueService(AppContext app) {
    super(app, app.db.queue, app.entityMapper.queue);
  }

  @Override
  public ApiObjectRef create(CreateQueueArg createArg, String routerRef)
      throws CommsRouterException {

    RouterObjectRef routerObjectRef =
        RouterObjectRef.builder().setRef(Uuid.get()).setRouterRef(routerRef).build();

    return app.db.transactionManager.execute((em) -> {
      return doCreate(em, createArg, routerObjectRef);
    });
  }

  @Override
  public ApiObjectRef replace(CreateQueueArg createArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.queue.delete(em, objectRef);
      em.flush();
      return doCreate(em, createArg, objectRef);
    });
  }

  private ApiObjectRef doCreate(EntityManager em, CreateQueueArg createArg,
      RouterObjectRef objectRef) throws CommsRouterException {

    app.db.router.lockConfigByRef(em, objectRef.getRouterRef());

    CommsRouterEvaluator evaluator =
        app.evaluatorFactory.provide(createArg.getPredicate(), objectRef.getRouterRef());
    evaluator.validate();

    Router router = getRouter(em, objectRef);
    Queue queue = new Queue(objectRef);
    queue.setRouter(router);
    queue.setDescription(createArg.getDescription());
    queue.setPredicate(createArg.getPredicate());
    em.persist(queue);
    attachAgents(em, queue, evaluator, true);
    return queue.cloneApiObjectRef();
  }

  private void attachAgents(EntityManager em, Queue queue, CommsRouterEvaluator evaluator,
      boolean isNewQueue) throws CommsRouterException {

    LOGGER.info("Queue {}: attaching agents...", queue.getRef());

    int attachedAgentsCount = 0;
    long millis = System.currentTimeMillis();
    List<Agent> agents = app.db.agent.list(em, queue.getRouter().getRef());
    for (Agent agent : agents) {
      try {
        if (evaluator.evaluate(agent.getCapabilities())) {

          LOGGER.info("Queue {} <=> Agent {}", queue.getRef(), agent.getRef());
          ++attachedAgentsCount;

          AgentQueueMapping mapping = new AgentQueueMapping(agent, queue);
          em.persist(mapping);

          if (isNewQueue || !agent.getAgentQueueMappings().contains(mapping)) {
            agent.getAgentQueueMappings().add(mapping);
          }
          queue.getAgentQueueMappings().add(mapping);
        } else if (!isNewQueue) {
          agent.getAgentQueueMappings().remove(new AgentQueueMapping(agent, queue));
        }
      } catch (CommsRouterException | RuntimeException ex) {
        LOGGER.error("Queue {}: failure attaching agent {}: {}", queue.getRef(), agent.getRef(), ex,
            ex);
        throw new ExpressionException(ex.getMessage(), ex);
      }
    }

    LOGGER.trace("Evaluate all agents attributes to queue predicate takes : {}",
        (System.currentTimeMillis() - millis));
    LOGGER.info("Queue {}: agents attached: {}", queue.getRef(), attachedAgentsCount);
  }

  @Override
  public void update(UpdateQueueArg updateArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Queue queue;
      if (updateArg.getPredicate() != null) {
        // ! get the queue after the router config lock
        app.db.router.lockConfigByRef(em, objectRef.getRouterRef());
        queue = app.db.queue.get(em, objectRef);
        checkResourceVersion(queue, objectRef);
        updatePredicate(em, queue, updateArg.getPredicate());
      } else {
        queue = app.db.queue.get(em, objectRef);
        checkResourceVersion(queue, objectRef);
      }
      Fields.update(queue::setDescription, queue.getDescription(), updateArg.getDescription());
    });
  }

  private void updatePredicate(EntityManager em, Queue queue, String predicate)
      throws CommsRouterException {

    if (Objects.equals(queue.getPredicate(), predicate)) {
      LOGGER.info("Queue {}: no predicate change - will keep current agents", queue.getRef());
      return;
    }
    LOGGER.info("Queue {}: detaching all agents due to predicate change", queue.getRef());

    CommsRouterEvaluator evaluator =
        app.evaluatorFactory.provide(predicate, queue.getRouter().getRef());
    evaluator.validate();

    queue.setPredicate(predicate);
    queue.getAgentQueueMappings().clear();
    attachAgents(em, queue, evaluator, false);
  }

  @Override
  public long getQueueSize(RouterObjectRef routerObjectRef) throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.queue.get(em, routerObjectRef); // Check that queue exists

      return app.db.queue.getQueueSize(em, routerObjectRef);
    });
  }

  @SuppressWarnings("unchecked")
  @Override
  public Collection<TaskDto> getTasks(RouterObjectRef routerObjectRef) throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.queue.get(em, routerObjectRef); // Check that queue exists

      String qlString = "SELECT t FROM Task t JOIN t.queue q JOIN q.router r "
          + "WHERE r.ref = :routerRef AND q.ref = :queueRef "
          + "AND t.state = :state ORDER BY t.priority DESC";

      List<Task> list = em.createQuery(qlString)
          .setParameter("routerRef", routerObjectRef.getRouterRef())
          .setParameter("queueRef", routerObjectRef.getRef())
          .setParameter("state", TaskState.waiting).getResultList();

      return app.entityMapper.task.toDto(list);
    });
  }

  @Override
  public void delete(RouterObjectRef routerObjectRef) throws CommsRouterException {
    app.db.transactionManager.executeVoid((em) -> {
      app.db.router.lockConfigByRef(em, routerObjectRef.getRouterRef());
      repository.delete(em, routerObjectRef);
    });
  }

}
