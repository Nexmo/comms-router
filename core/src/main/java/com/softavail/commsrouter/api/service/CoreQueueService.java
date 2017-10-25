package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.QueueService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.Collection;
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

  @Override
  public void update(UpdateQueueArg updateArg, RouterObjectId objectId)
      throws CommsRouterException {

    final String newPredicate = updateArg.getPredicate();
    app.db.transactionManager.executeVoid((em) -> {
      Queue queue = app.db.queue.get(em, objectId.getId());
      List<Agent> matchedAgents = new ArrayList<>();
      if (newPredicate != null && !newPredicate.isEmpty()) {
        List<Agent> agents = app.db.agent.list(em, objectId.getRouterId());
        agents.forEach((agent) -> {
          try {
            if (app.evaluator.evaluateAgentCapabilitiesForQueue(agent.getId(),
                app.entityMapper.attributes.toDto(agent.getCapabilities()), queue)) {
              matchedAgents.add(agent);
            }
          } catch (CommsRouterException ex) {
            LOGGER.warn("Evaluation for Agent with ID={} failed : {}", agent.getId(),
                ex.getLocalizedMessage());
          }
        });
        if (matchedAgents.isEmpty()) {
          LOGGER.warn("Queue with ID={} didn't match to any agent capabilities.", queue.getId());
        }
      } else {
        queue.getAgents().clear();
        LOGGER.warn("Queue with ID={} cleared all assigned Agents.", queue.getId());
      }
      Fields.update(queue::setAgents, queue.getAgents(), matchedAgents);
      Fields.update(queue::setDescription, queue.getDescription(), updateArg.getDescription());
      Fields.update(queue::setPredicate, queue.getPredicate(), updateArg.getPredicate());
    });
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

  private ApiObjectId doCreate(EntityManager em, CreateQueueArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    app.evaluator.isValidExpression(createArg.getPredicate());
    Queue queue = new Queue(createArg, objectId);

    if (objectId.getRouterId() != null) {
      List<Agent> agents = app.db.agent.list(em, objectId.getRouterId());
      agents.forEach((agent) -> {
        try {
          if (app.evaluator.evaluateAgentCapabilitiesForQueue(agent.getId(),
              app.entityMapper.attributes.toDto(agent.getCapabilities()), queue)) {
            queue.getAgents().add(agent);
          }
        } catch (CommsRouterException ex) {
          LOGGER.warn("Evaluation for Agent with ID={} failed : {}", agent.getId(),
              ex.getLocalizedMessage());
        }
      });
    }

    if (queue.getAgents().isEmpty()) {
      LOGGER.warn("Queue with ID={} didn't match to any agent capabilities.", queue.getId());
    }

    em.persist(queue);
    QueueDto queueDto = entityMapper.toDto(queue);
    return new ApiObjectId(queueDto);
  }

}
