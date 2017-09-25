package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.QueueService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.util.Fields;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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
  public QueueDto create(CreateQueueArg createArg) throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      Queue queue = new Queue(ensureIdPresent(createArg));

      if (createArg.getRouterId() != null) {
        List<Agent> agents = app.db.agent.list(em, createArg.getRouterId());
        for (Agent agent : agents) {
          if (app.evaluator.evaluateAgentCapabilitiesForQueue(agent.getId(),
              app.entityMapper.attributes.toDto(agent.getCapabilities()), queue)) {
            queue.getAgents().add(agent);
          }
        }
      }

      if (queue.getAgents().isEmpty()) {
        LOGGER.warn("Queue with ID={} didn't match to any agent capabilities.", queue.getId());
      }

      em.persist(queue);
      return entityMapper.toDto(queue);
    });
  }

  @Override
  public void update(UpdateQueueArg updateArg) throws CommsRouterException {

    final String newPredicate = updateArg.getPredicate();
    app.db.transactionManager.executeVoid((em) -> {
      Queue queue = app.db.queue.get(em, updateArg);
      List<Agent> matchedAgents = new ArrayList<>();
      if (newPredicate != null && !newPredicate.isEmpty()) {
        List<Agent> agents = app.db.agent.list(em, updateArg.getRouterId());
        for (Agent agent : agents) {
          if (app.evaluator.evaluateAgentCapabilitiesForQueue(agent.getId(),
              app.entityMapper.attributes.toDto(agent.getCapabilities()), queue)) {
            matchedAgents.add(agent);
          }
        }
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
  public long getQueueSize(RouterObject routerObject) throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.queue.get(em, routerObject); // Check that queue exists

      String qlString = "SELECT COUNT(t.id) FROM Task t "
          + "JOIN t.queue q WHERE q.id = :queueId AND t.state = :state";

      return (long) em.createQuery(qlString).setParameter("queueId", routerObject.getId())
          .setParameter("state", TaskState.waiting).getSingleResult();
    });
  }

  @SuppressWarnings("unchecked")
  @Override
  public Collection<TaskDto> getTasks(RouterObject routerObject) throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.queue.get(em, routerObject); // Check that queue exists

      String qlString =
          "SELECT t FROM Task t JOIN t.queue q WHERE q.id = :queueId AND t.state = :state";

      List<Task> list = em.createQuery(qlString).setParameter("queueId", routerObject.getId())
          .setParameter("state", TaskState.waiting).getResultList();

      return app.entityMapper.task.toDto(list);
    });
  }

}
