/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CoreAgentServiceJpaTest extends TestBase {

    private static final Logger LOGGER = LogManager.getLogger(CoreRouterServiceJpaTest.class);

    @Test
    public void createTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        queueService.create(newCreateQueueArg("predicate_one", "description_one"), id);
        agentService.create(returnNewCreateAgentArg("address_one"), id);
        AgentDto agent = agentService.get(id);

        assertEquals(agent.getAddress(),"address_one");
    }

    @Test
    public void putTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        agentService.create(returnNewCreateAgentArg("address_one"), id);

        AgentDto agent = agentService.put(returnNewCreateAgentArg("address_two"), id);

        assertEquals(agent.getAddress(),"address_two");
    }

    @Test
    public void updateTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        agentService.create(returnNewCreateAgentArg("address_one"), id);

        agentService.update(returnNewUpdateAgentArg("address_two", AgentState.ready), id);

        AgentDto agent = agentService.get(id);
        
        assertEquals(agent.getAddress(), "address_two");
        assertEquals(agent.getState(),AgentState.ready);
    }

    @Test(expected=BadValueException.class)
    public void updateStateBusy() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        agentService.create(returnNewCreateAgentArg("address_one"), id); //offline
        agentService.update(returnNewUpdateAgentArg("address_two", AgentState.busy), id); //can't be busy
    }

    @Test
    public void updateStateOffline() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        agentService.create(returnNewCreateAgentArg("address_one"), id); //offline
        agentService.update(returnNewUpdateAgentArg("address_two", AgentState.ready), id); //ready
        agentService.update(returnNewUpdateAgentArg("address_two", AgentState.offline), id); //offline
        AgentDto agent = agentService.get(id);
        assertEquals(agent.getState(),AgentState.offline);
    }








    
}
