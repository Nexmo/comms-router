/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CoreAgentServiceJpaTest extends TestBase {

    //Testing the create method that takes a RouterObjectId
    @Test
    public void createTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        queueService.create(newCreateQueueArg("1=1", "description_one"), id);
        agentService.create(newCreateAgentArg("address_one"), id);
        AgentDto agent = agentService.get(id);
        assertEquals(agent.getAddress(),"address_one");
    }

    //Testing the create method that takes a String routerId
    @Test
    public void createTestTwo() throws CommsRouterException {
        queueService.create(newCreateQueueArg("1=1", "description_one"), "01");
        agentService.create(newCreateAgentArg("address_one"), "01");
        List<AgentDto> agent = agentService.list("01");
        assertEquals(agent.get(0).getAddress(), "address_one");
    }

    //Testing the update method
    @Test
    public void updateTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        agentService.create(newCreateAgentArg("address_one"), id);
        agentService.update(newUpdateAgentArg("address_two", AgentState.ready), id);
        AgentDto agent = agentService.get(id);
        assertEquals(agent.getAddress(), "address_two");
        assertEquals(agent.getState(),AgentState.ready);
    }

    //Setting state to busy
    @Test(expected=BadValueException.class)
    public void updateStateBusy() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        agentService.create(newCreateAgentArg("address_one"), id); //offline
        agentService.update(newUpdateAgentArg("address_two", AgentState.busy), id); //can't be busy
    }

    //Updating state to offline
    @Test
    public void updateStateOffline() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        agentService.create(newCreateAgentArg("address_one"), id); //offline
        agentService.update(newUpdateAgentArg("address_two", AgentState.ready), id); //ready
        agentService.update(newUpdateAgentArg("address_two", AgentState.offline), id); //offline
        AgentDto agent = agentService.get(id);
        assertEquals(agent.getState(),AgentState.offline);
    }
    
}
