package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
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
  public AgentDto create(CreateAgentArg createArg) throws CommsRouterException {

    return app.db.transactionManager.execute((EntityManager em) -> {
      Agent agent = new Agent(ensureIdPresent(createArg));
      agent.setAddress(createArg.getAddress());
      agent.setCapabilities(app.entityMapper.attributes.toJpa(createArg.getCapabilities()));
      agent.setState(createArg.getState());

      if (createArg.getRouterId() != null) {
        List<Queue> queues = app.db.queue.list(em, createArg.getRouterId());
        for (Queue queue : queues) {
          try {
            if (app.evaluator.evaluateNewAgentForQueue(createArg, queue)) {
              agent.getQueues().add(queue);
            }
          } catch (CommsRouterException ex) {
            LOGGER.warn("Evaluation for Queue with ID={} failed : {}", queue.getId(),
                ex.getLocalizedMessage());
          }
        }
      }

      if (agent.getQueues().isEmpty()) {
        LOGGER.warn("Agent with ID={} didn't match to any queues.", agent.getId());
      }

      em.persist(agent);
      return app.entityMapper.agent.toDto(agent);
    });


  }

  @Override
  public void update(UpdateAgentArg updateArg) throws CommsRouterException {

    boolean becameReady = updateAgent(updateArg);
    if (becameReady) {
      app.taskDispatcher.dispatchAgent(updateArg.getId());
    }

  }

  private Boolean compareCapabilities(AttributeGroupDto newAttributes,
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

      // TODO: @EAS: add check by Value equals ...

    }
    return false; // !keys.isEmpty();
  }

  private boolean updateAgent(UpdateAgentArg updateArg) throws CommsRouterException {

    if (updateArg.getState() == AgentState.busy) {
      throw new BadValueException("Setting agent state to busy not allowed");
    }

    AttributeGroupDto newAttributes = updateArg.getCapabilities();
    return app.db.transactionManager.execute((em) -> {
      Agent agent = app.db.agent.get(em, updateArg);
      final AttributeGroupDto oldAttributes =
          app.entityMapper.attributes.toDto(agent.getCapabilities());
      AgentState oldState = agent.getState();
      boolean agentBecameAvailabe;
      if (oldState == updateArg.getState()) {
        agentBecameAvailabe = false;
      } else {
        switch (oldState) {
          case busy:
            throw new InvalidStateException("Changing state of a busy agent is not implemented");
          case offline:
            // check once again just in case
            agentBecameAvailabe = updateArg.getState() == AgentState.ready;
            break;
          case ready:
            agentBecameAvailabe = false;
            break;
          default:
            throw new InternalErrorException("Unexpected agent state");
        }
      }
      if (!compareCapabilities(newAttributes, oldAttributes)) {
        List<Queue> matchedQueues = new ArrayList<>();
        List<Queue> queues = app.db.queue.list(em, updateArg.getRouterId());
        for (Queue queue : queues) {
          try {
            if (app.evaluator.evaluateUpdateAgentForQueue(updateArg, queue)) {
              matchedQueues.add(queue);
            }
          } catch (CommsRouterException ex) {
            LOGGER.warn("Evaluation for Queue with ID={} failed : {}", queue.getId(),
                ex.getLocalizedMessage());
          }
        }
        if (matchedQueues.isEmpty()) {
          LOGGER.warn("Agent with ID={} didn't match to any queues.", agent.getId());
        }

        Fields.update(agent::setQueues, agent.getQueues(), matchedQueues);
        agent.removeCapabilities();
      }
      Fields.update(agent::setAddress, agent.getAddress(), updateArg.getAddress());
      if (newAttributes != null) {
        Fields.update(agent::setCapabilities, agent.getCapabilities(),
            app.entityMapper.attributes.toJpa(newAttributes));
      }
      Fields.update(agent::setState, agent.getState(), updateArg.getState());
      return agentBecameAvailabe;
    });
  }

}
