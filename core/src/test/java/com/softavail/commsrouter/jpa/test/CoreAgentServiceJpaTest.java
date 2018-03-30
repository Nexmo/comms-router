/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import static org.junit.Assert.assertEquals;

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;

import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CoreAgentServiceJpaTest extends TestBase {

  // Testing the replace method that takes a RouterObjectRef
  @Test
  public void createTest() throws CommsRouterException {
    String routerRef = "router-ref";
    routerService.replace(newCreateRouterArg("router-name", ""), routerRef);
    RouterObjectRef ref = new RouterObjectRef("ref", routerRef);
    queueService.replace(newCreateQueueArg("1==1", "description_one"), ref);
    agentService.replace(newCreateAgentArg("address_one"), ref);
    AgentDto agent = agentService.get(ref);
    assertEquals(agent.getAddress(), "address_one");
  }

  // Testing the replace method that takes a String routerId
  @Test
  public void createTestTwo() throws CommsRouterException {
    String routerRef = "router-ref";
    routerService.replace(newCreateRouterArg("router-name", ""), routerRef);
    queueService.create(newCreateQueueArg("1==1", "description_one"), routerRef);
    agentService.create(newCreateAgentArg("address_one"), routerRef);
    PaginatedList<AgentDto> agent = agentService.list(
        new PagingRequest(routerRef, null, 10, null,null));
    assertEquals(agent.getList().get(0).getAddress(), "address_one");
  }

  // Testing the update method
  @Test
  public void updateTest() throws CommsRouterException {
    RouterObjectRef ref = new RouterObjectRef("aktyriskghsirol", "01");
    queueService.replace(newCreateQueueArg("1==1", "description_one"), ref);
    agentService.replace(newCreateAgentArg("address_one"), ref);
    // Updating
    AgentDto beforeUpdate = agentService.get(ref);
    agentService.update(newUpdateAgentArg("address_two", AgentState.ready), beforeUpdate);
    AgentDto agent = agentService.get(ref);
    assertEquals(agent.getAddress(), "address_two");
    assertEquals(agent.getState(), AgentState.ready);
  }

  // Setting state to busy. Expecting a BadValueException
  @Test(expected = BadValueException.class)
  public void updateStateBusy() throws CommsRouterException {
    RouterObjectRef ref = new RouterObjectRef("", "01");
    agentService.replace(newCreateAgentArg("address_one"), ref); // offline
    agentService.update(newUpdateAgentArg("address_two", AgentState.busy), ref); // can't be busy
  }

  // Updating state to offline
  @Test
  public void updateStateOffline() throws CommsRouterException {
    RouterObjectRef ref = new RouterObjectRef("", "01");
    agentService.replace(newCreateAgentArg("address_one"), ref); // offline
    AgentDto beforeUpdate = agentService.get(ref);
    agentService.update(newUpdateAgentArg("address_two", AgentState.ready), beforeUpdate); // ready
    beforeUpdate = agentService.get(ref);
    agentService.update(newUpdateAgentArg("address_two", AgentState.offline), beforeUpdate); // offline
    AgentDto agent = agentService.get(ref);
    assertEquals(agent.getState(), AgentState.offline);
  }

}
