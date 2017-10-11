/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.service.CoreAgentService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.app.TaskDispatcher;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import static org.junit.Assert.assertEquals;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CoreAgentServiceJpaTest extends TestBase {

    private static CoreAgentService agentService;
    private static AppContext app;

    private static final Logger LOGGER = LogManager.getLogger(CoreRouterServiceJpaTest.class);

    public CreateAgentArg returnNewCreateAgentArg(String address) {
        AttributeGroupDto aGroupDto = new AttributeGroupDto();
        CreateAgentArg args = new CreateAgentArg();
        args.setAddress(address);
        args.setCapabilities(aGroupDto);
        return args;
    }

     public UpdateAgentArg returnNewUpdateAgentArg(String address) {
        AttributeGroupDto aGroupDto = new AttributeGroupDto();
        UpdateAgentArg args = new UpdateAgentArg();
        args.setAddress(address);
        args.setCapabilities(aGroupDto);
        return args;
    }
    
    @BeforeClass
    public static void setTestCoreAgentService() {

        CommsRouterEvaluator ev = new CommsRouterEvaluator();
        JpaDbFacade db = new JpaDbFacade("mnf-pu-test");
        TaskDispatcher td = new TaskDispatcher(null, null, null);
        EntityMappers enm = new EntityMappers();
        app = new AppContext(db, ev, td, enm);
        agentService = new CoreAgentService(app);

    }

    @Test
    public void createTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("id_one", "01");
        agentService.create(returnNewCreateAgentArg("address_one"), id);
        AgentDto agent = agentService.get(id);

        assertEquals(agent.getAddress(),"address_one");
    }

    @Test
    public void putTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("id_one", "01");
        agentService.create(returnNewCreateAgentArg("address_one"), id);

        AgentDto agent = agentService.put(returnNewCreateAgentArg("address_two"), id);

        assertEquals(agent.getAddress(),"address_two");
    }

    @Test
    public void updateTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("id_one", "01");
        agentService.create(returnNewCreateAgentArg("address_one"), id);

        agentService.update(returnNewUpdateAgentArg("address_two"), id);

        AgentDto agent = agentService.get(id);
        
        assertEquals(agent.getAddress(), "address_two");
    }







    
}
