/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
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

  // Testing the create method of the CorePlanService class
  @Test
  public void createTest() throws CommsRouterException {
    RouterObjectId id = new RouterObjectId("plan-id", "router-id");
    String queueId = "queueId_one";
    queueService.create(newCreateQueueArg("1==1", "queue 1"), new RouterObjectId(queueId, id));
    planService.create(newCreatePlanArg("desctiption_one", "1==1", queueId), id);
    PlanDto createdPlan = planService.get(id);
    assertEquals(createdPlan.getDescription(), "desctiption_one");
  }

  // Testing the update method of the CorePlanService class
  @Test
  public void updateTest() throws CommsRouterException {
    String queueId1 = "queueId_one";
    String queueId2 = "queueId_two";
    RouterObjectId id = new RouterObjectId("plan-id", "router-id");
    queueService.create(newCreateQueueArg("1==1", "queue 1"), new RouterObjectId(queueId1, id));
    queueService.create(newCreateQueueArg("1==1", "queue 2"), new RouterObjectId(queueId2, id));
    planService.create(newCreatePlanArg("desctiption_one", "1==1", queueId1), id);
    planService.update(newUpdatePlanArg("desctiption_two", "1==1", queueId2), id);
    PlanDto updatedPlan = planService.get(id);
    List<RuleDto> rules = updatedPlan.getRules();
    assertEquals(updatedPlan.getDescription(), "desctiption_two");
    assertEquals(rules.get(0).getRoutes().get(0).getQueueId(), "queueId_two");
  }

  // Testing method list from CoreRouterObjectService
  @Test
  public void listTest() throws CommsRouterException {
    String queueId = "queueId_one";
    String routerId = "router-id";
    queueService.create(newCreateQueueArg("1==1", "queue 1"),
        new RouterObjectId(queueId, routerId));
    planService.create(newCreatePlanArg("desctiption_one", "1==1", queueId), routerId);
    planService.create(newCreatePlanArg("desctiption_two", "1==1", queueId), routerId);
    List<PlanDto> plans = planService.list(routerId);
    assertEquals(plans.size(), 2);
  }

  // Testing method delete from CoreRouterObjectService
  @Test
  public void deleteTest() throws CommsRouterException {
    String queueId = "queueId_one";
    RouterObjectId id = new RouterObjectId("plan-id", "router-id");
    queueService.create(newCreateQueueArg("1==1", "queue 1"), new RouterObjectId(queueId, id));
    planService.create(newCreatePlanArg("desctiption_one", "1==1", queueId), id);
    List<PlanDto> plans = planService.list(id.getRouterId());
    assertEquals(plans.size(), 1);
    planService.delete(id);
    plans = planService.list(id.getRouterId());
    assertEquals(plans.size(), 0);
  }

  // Testing PaginatedList (list method from CoreRouterService)
  @Test
  public void listPagesTest() throws CommsRouterException {
    String queueId = "queueId_one";
    RouterObjectId id = new RouterObjectId("plan-id", "router-id");
    queueService.create(newCreateQueueArg("1==1", "queue 1"), new RouterObjectId(queueId, id));
    ApiObjectId plan = planService.create(newCreatePlanArg("desctiption_one", "1==1", queueId), id);
    PaginatedList<PlanDto> list = planService.list("router-id", 0, 0);
  }

}
