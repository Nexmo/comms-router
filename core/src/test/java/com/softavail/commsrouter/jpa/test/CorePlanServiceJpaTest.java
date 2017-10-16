/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CorePlanServiceJpaTest extends TestBase {

    private static final Logger LOGGER = LogManager.getLogger(CorePlanServiceJpaTest.class);

    //Testing the create method of the CorePlanService class
    @Test
    public void createTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("thisIsAnId_one","routerId_one");
        planService.create(returnNewCreatePlanArg("desctiption_one","predicate_one","queueId_one"), id);
        PlanDto createdPlan = planService.get(id);
        
        assertEquals(createdPlan.getDescription(), "desctiption_one");
    }

    //Testing the put method of the CorePlanService class
    @Test
    public void putTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("thisIsAnId_one","routerId_one");
        planService.create(returnNewCreatePlanArg("desctiption_one","predicate_one","queueId_one"), id);
        planService.put(returnNewCreatePlanArg("desctiption_two","predicate_two","queueId_two"), id);
        PlanDto createdPlan = planService.get(id);
        List<RuleDto> rules = createdPlan.getRules();
        
        assertEquals(createdPlan.getDescription(), "desctiption_two");
        assertEquals(rules.get(0).getPredicate(), "predicate_two");
        assertEquals(rules.get(0).getQueueId(), "queueId_two");
    }

    //Testing the update method of the CorePlanService class
    @Test
    public void updateTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("thisIsAnId_one","routerId_one");
        planService.create(returnNewCreatePlanArg("desctiption_one","predicate_one","queueId_one"), id);
        planService.update(returnNewUpdatePlanArg("desctiption_two","predicate_two","queueId_two"), id);
        PlanDto updatedPlan = planService.get(id);
        List<RuleDto> rules = updatedPlan.getRules();
        
        assertEquals(updatedPlan.getDescription(), "desctiption_two");
        assertEquals(rules.get(0).getPredicate(), "predicate_two");
        assertEquals(rules.get(0).getQueueId(), "queueId_two");
    }

    //Testing method list from CoreRouterObjectSercie
    @Test
    public void listTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("thisIsAnId_one","routerId_one");
        planService.create(returnNewCreatePlanArg("desctiption_one","predicate_one","queueId_one"), id);
        id.setId("thisIsAnId_two");
        planService.create(returnNewCreatePlanArg("desctiption_two","predicate_two","queueId_two"), id);
        List<PlanDto> plans = planService.list("routerId_one");
        assertEquals(plans.size(),2);
    }

    //Testing method delete from CoreRouterObjectSercie
    @Test
    public void deleteTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("thisIsAnId_one","routerId_one");
        planService.create(returnNewCreatePlanArg("desctiption_one","predicate_one","queueId_one"), id);
        List<PlanDto> plans = planService.list("routerId_one");
        assertEquals(plans.size(),1);
        planService.delete(id);
        plans = planService.list("routerId_one");
        assertEquals(plans.size(),0);
    }

    @Test
    public void getDtoEntityTest() {
        Class<PlanDto> newPlan = planService.getDtoEntityClass();
    }

    @Test
    public void listPagesTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("thisIsAnId_one","01");
        PlanDto plan = planService.create(returnNewCreatePlanArg("desctiption_one","predicate_one","queueId_one"), id);
        PaginatedList<PlanDto> list = planService.listPage("01", 0, 0);
    }

}
