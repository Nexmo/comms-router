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
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ReferenceIntegrityViolationException;
import com.softavail.commsrouter.api.interfaces.QueueService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;
import java.util.Collection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
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
  public ApiObjectId create(CreateQueueArg createArg, String routerId)
      throws CommsRouterException {

    RouterObjectId routerObjectId = RouterObjectId.builder()
        .setId(Uuid.get())
        .setRouterId(routerId)
        .build();

    return app.db.transactionManager.execute((em) -> {
      return doCreate(em, createArg, routerObjectId);
    });
  }

  @Override
  public ApiObjectId create(CreateQueueArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.queue.delete(em, objectId.getId());
      return doCreate(em, createArg, objectId);
    });
  }

  private ApiObjectId doCreate(EntityManager em, CreateQueueArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    app.evaluator.isValidExpression(createArg.getPredicate());

    Router router = getRouter(em, objectId);
    Queue queue = new Queue(objectId);
    queue.setRouter(router);
    queue.setDescription(createArg.getDescription());
    queue.setPredicate(createArg.getPredicate());
    attachAgents(em, queue, true);
    em.persist(queue);
    return queue.cloneApiObjectId();
  }

  private void attachAgents(EntityManager em, Queue queue, boolean isNewQueue) {

    LOGGER.info("Queue {}: attaching agents...", queue.getId());

    int attachedAgentsCount = 0;

    for (Agent agent : app.db.agent.list(em, queue.getRouter().getId())) {
      try {
        if (app.evaluator.evaluate(app.entityMapper.attributes.toDto(agent.getCapabilities()),
            queue.getPredicate())) {

          LOGGER.info("Queue {} <=> Agent {}", queue.getId(), agent.getId());
          ++attachedAgentsCount;

          if (isNewQueue || !agent.getQueues().contains(queue)) {
            agent.getQueues().add(queue);
          }
          queue.getAgents().add(agent);
        } else if (!isNewQueue) {
          agent.getQueues().remove(queue);
        }
      } catch (CommsRouterException ex) {
        LOGGER.error("Queue {}: failure attaching agent {}: {}", queue.getId(), agent.getId(), ex,
            ex);
      }
    }
    LOGGER.info("Queue {}: agents attached: {}", queue.getId(), attachedAgentsCount);
  }

  @Override
  public void update(UpdateQueueArg updateArg, RouterObjectId objectId)
      throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Queue queue = app.db.queue.get(em, objectId.getId());
      updatePredicate(em, queue, updateArg.getPredicate());
      Fields.update(queue::setDescription, queue.getDescription(), updateArg.getDescription());
    });
  }

  private void updatePredicate(EntityManager em, Queue queue, String predicate) {

    if (predicate == null) {
      // no change requested
      return;
    }

    if (queue.getPredicate().equals(predicate)) {
      LOGGER.info("Queue {}: no predicate change - will keep current agents", queue.getId());
      return;
    }
    LOGGER.info("Queue {}: detaching all agents due to predicate change", queue.getId());

    queue.setPredicate(predicate);
    queue.getAgents().clear();
    attachAgents(em, queue, false);
  }

  @Override
  public long getQueueSize(RouterObjectId routerObjectId)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.queue.get(em, routerObjectId); // Check that queue exists

      return app.db.queue.getQueueSize(em, routerObjectId.getId());
    });
  }

  @SuppressWarnings("unchecked")
  @Override
  public Collection<TaskDto> getTasks(RouterObjectId routerObjectId)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.queue.get(em, routerObjectId); // Check that queue exists

      String qlString = "SELECT t FROM Task t JOIN t.queue q WHERE q.id = :queueId "
          + "AND t.state = :state ORDER BY t.priority DESC";

      List<Task> list = em.createQuery(qlString)
          .setParameter("queueId", routerObjectId.getId())
          .setParameter("state", TaskState.waiting)
          .getResultList();

      return app.entityMapper.task.toDto(list);
    });
  }

  @Override
  public void delete(RouterObjectId routerObjectId) throws CommsRouterException {
    try {
      super.delete(routerObjectId);
    } catch (ReferenceIntegrityViolationException ex) {
      throw ex;
    }
  }

}
