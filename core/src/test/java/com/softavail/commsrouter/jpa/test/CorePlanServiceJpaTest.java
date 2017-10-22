/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CorePlanServiceJpaTest extends TestBase {

    //Testing the create method of the CorePlanService class
    @Test
    public void createTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("","01");
        planService.create(newCreatePlanArg("desctiption_one","1=1","queueId_one"), id);
        PlanDto createdPlan = planService.get(id);
        assertEquals(createdPlan.getDescription(), "desctiption_one");
    }

    //Testing the update method of the CorePlanService class
    @Test
    public void updateTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("","01");
        planService.create(newCreatePlanArg("desctiption_one","1=1","queueId_one"), id);
        planService.update(newUpdatePlanArg("desctiption_two","1=1","queueId_two"), id);
        PlanDto updatedPlan = planService.get(id);
        List<RuleDto> rules = updatedPlan.getRules();
        assertEquals(updatedPlan.getDescription(), "desctiption_two");
        assertEquals(rules.get(0).getQueueId(), "queueId_two");
    }

    //Testing method list from CoreRouterObjectService
    @Test
    public void listTest() throws CommsRouterException {
        planService.create(newCreatePlanArg("desctiption_one","1=1","queueId_one"), "01"); 
        planService.create(newCreatePlanArg("desctiption_two","1=1","queueId_one"), "01");
        List<PlanDto> plans = planService.list("01");
        assertEquals(plans.size(),2);
    }

    //Testing method delete from CoreRouterObjectService
    @Test
    public void deleteTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("id","01");
        planService.create(newCreatePlanArg("desctiption_one","1=1","queueId_one"), id);
        List<PlanDto> plans = planService.list("01");
        assertEquals(plans.size(),1);
        planService.delete(id);
        plans = planService.list("routerId_one");
        assertEquals(plans.size(),0);
    }

    //Testing PaginatedList (list method from CoreRouterService)
    @Test
    public void listPagesTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("","01");
        ApiObjectId plan = planService.create(newCreatePlanArg("desctiption_one","1=1","queueId_one"), id);
        PaginatedList<PlanDto> list = planService.list("01", 0, 0);
    }

}
