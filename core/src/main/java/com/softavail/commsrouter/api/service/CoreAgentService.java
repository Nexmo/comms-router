package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.InternalErrorException;
import com.softavail.commsrouter.api.exception.InvalidStateException;
import com.softavail.commsrouter.api.interfaces.AgentService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.Set;
import javax.persistence.EntityManager;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class CoreAgentService extends CoreRouterObjectService<AgentDto, Agent>
    implements AgentService {

  private static final Logger LOGGER = LogManager.getLogger(CoreAgentService.class);

  public CoreAgentService(AppContext app) {
    super(app, app.db.agent, app.entityMapper.agent);
  }

  @Override
  public ApiObjectId create(CreateAgentArg createArg, String routerId)
      throws CommsRouterException {

    RouterObjectId routerObjectId = RouterObjectId.builder()
        .setId(Uuid.get())
        .setRouterId(routerId)
        .build();

    return app.db.transactionManager.execute((EntityManager em) -> {
      return doCreate(em, createArg, routerObjectId);
    });
  }

  @Override
  public ApiObjectId create(CreateAgentArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.agent.delete(em, objectId.getId());
      return doCreate(em, createArg, objectId);
    });
  }

  @Override
  public void update(UpdateAgentArg updateArg, RouterObjectId objectId)
      throws CommsRouterException {

    boolean becameReady = updateAgent(updateArg, objectId);
    if (becameReady) {
      app.taskDispatcher.dispatchAgent(objectId.getId());
    }

  }

  private Boolean capabilitiesAreEqual(AttributeGroupDto newAttributes,
      AttributeGroupDto oldAttributes) {
    if (newAttributes == null && oldAttributes != null) {
      return false;
    }
    if (newAttributes != null && oldAttributes == null) {
      return false;
    }
    if (newAttributes == null && oldAttributes == null) {
      return true;
    }
    if (newAttributes.size() != oldAttributes.size()) {
      return false;
    }
    Set<String> keys = newAttributes.keySet();
    for (String key : keys) {
      if (!oldAttributes.containsKey(key)) {
        return false;
      }
    }
    return false; // !keys.isEmpty();
  }

  private boolean updateAgent(UpdateAgentArg updateArg, RouterObjectId objectId)
      throws CommsRouterException {

    if (updateArg.getState() == AgentState.busy
        || updateArg.getState() == AgentState.unavailable) {
      throw new BadValueException(
          "Setting agent state to '" + updateArg.getState() + "' not allowed");
    }

    return app.db.transactionManager.execute((em) -> {
      Agent agent = app.db.agent.get(em, objectId.getId());
      boolean agentBecameAvailable = updateState(agent, updateArg.getState());
      updateCapabilitiesAndQueues(em, agent, updateArg.getCapabilities());
      Fields.update(agent::setAddress, agent.getAddress(), updateArg.getAddress());
      return agentBecameAvailable;
    });
  }

  private boolean updateState(Agent agent, AgentState newState)
      throws InvalidStateException, InternalErrorException {
    if (newState == null) {
      // no change requested
      return false;
    }
    final AgentState oldState = agent.getState();
    if (oldState == newState) {
      return false;
    }
    boolean agentBecameAvailable = false;
    switch (oldState) {
      case busy:
        throw new InvalidStateException(
            "Changing state of a busy agent is not implemented. Complete corresponding task.");
      case offline:
      case unavailable:
        // check once again just in case
        agentBecameAvailable = newState == AgentState.ready;
        break;
      case ready:
        agentBecameAvailable = false;
        break;
      default:
        throw new InternalErrorException("Unexpected agent state");
    }
    agent.setState(newState);
    return agentBecameAvailable;
  }

  private void updateCapabilitiesAndQueues(EntityManager em, Agent agent,
      AttributeGroupDto newCapabilities) {

    if (newCapabilities == null) {
      // no capabilities change requested
      return;
    }

    final AttributeGroupDto oldCapabilities =
        app.entityMapper.attributes.toDto(agent.getCapabilities());

    if (capabilitiesAreEqual(newCapabilities, oldCapabilities)) {
      return;
    }

    agent.setCapabilities(app.entityMapper.attributes.fromDto(newCapabilities));

    agent.getQueues().clear();

    boolean aQueueMatched = false;

    for (Queue queue : app.db.queue.list(em, agent.getRouterId())) {
      try {
        if (app.evaluator.evaluatePredicateByAttributes(newCapabilities,
            queue.getPredicate())) {
          LOGGER.info("Update agent with ID={} matched to queue with ID={}", agent.getId(),
              queue.getId());

          aQueueMatched = true;
          if (!queue.getAgents().contains(agent)) {
            queue.getAgents().add(agent);
          }
          agent.getQueues().add(queue);
        } else {
          queue.getAgents().remove(agent);
        }
      } catch (CommsRouterException ex) {
        LOGGER.warn("Evaluation for Queue with ID={} failed: {}", queue.getId(), ex, ex);
      }
    }

    if (!aQueueMatched) {
      LOGGER.warn("Agent with ID={} didn't match to any queues.", agent.getId());
    }
  }

  private ApiObjectId doCreate(EntityManager em, CreateAgentArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    Agent agent = new Agent(objectId);
    agent.setAddress(createArg.getAddress());
    agent.setCapabilities(app.entityMapper.attributes.fromDto(createArg.getCapabilities()));
    agent.setState(AgentState.offline);

    if (objectId.getRouterId() != null) {
      List<Queue> queues = app.db.queue.list(em, objectId.getRouterId());
      queues.forEach((queue) -> {
        try {
          if (app.evaluator.evaluatePredicateByAttributes(createArg.getCapabilities(),
              queue.getPredicate())) {
            LOGGER.info("Create agent with ID={} matched to queue with ID={}", objectId.getId(),
                queue.getId());
            agent.getQueues().add(queue);
            queue.getAgents().add(agent);
          }
        } catch (CommsRouterException ex) {
          LOGGER.warn("Evaluation for Queue with ID={} failed : {}", queue.getId(),
              ex.getLocalizedMessage());
          }
      });
    }

    if (agent.getQueues().isEmpty()) {
      LOGGER.warn("Agent with ID={} didn't match to any queues.", agent.getId());
    }

    em.persist(agent);

    return new ApiObjectId(app.entityMapper.agent.toDto(agent));
  }

}
