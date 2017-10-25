/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package com.softavail.commsrouter.eval;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Route;
import com.softavail.commsrouter.domain.Rule;
import java.net.MalformedURLException;
import java.net.URL;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author ergyunsyuleyman
 */
public class CommsRouterEvaluatorTest {
  CreateTaskArg createTaskArg;
  CreateAgentArg createAgentArg;
  UpdateAgentArg updateAgentArg;
  AttributeGroupDto requirements;
  Rule rule;
  Queue queue;
  String agentId;
  String taskId;
  String predicate1;
  String predicate2;
  String predicate3;

  public CommsRouterEvaluatorTest() {
    createTaskArg = new CreateTaskArg();
    createAgentArg = new CreateAgentArg();
    updateAgentArg = new UpdateAgentArg();
    requirements = new AttributeGroupDto();

    rule = new Rule();
    queue = new Queue();
    taskId = "task-id1";
    agentId = "agent-id1";
    predicate1 = "#{language} == 'en'";
    predicate2 = "IN(#{language}, ['es', 'en', 'fr']) || #{color}=='brown'";
    predicate3 = "CONTAINS(#{nickname}, 'Sto') && HAS(['es', 'en', 'fr'], 'de') && #{color}=='red'";
  }

  @BeforeClass
  public static void setUpClass() {}

  @AfterClass
  public static void tearDownClass() {}

  @Before
  public void setUp() {
    requirements.put("language", new StringAttributeValueDto("en"));
    requirements.put("nickname", new StringAttributeValueDto("The Stone"));
    requirements.put("color", new StringAttributeValueDto("red"));
    requirements.put("price", new DoubleAttributeValueDto(42D));
    createTaskArg.setRequirements(requirements);
    try {
      createTaskArg.setCallbackUrl(new URL("https://localhost:8084"));
    } catch (MalformedURLException ex) {
      throw new RuntimeException("Bad URL");
    }

    createAgentArg.setAddress("sip:someone@somesip.pip");
    createAgentArg.setCapabilities(requirements);

    updateAgentArg.setAddress("sip:someone2@somesip.pip");
    updateAgentArg.setCapabilities(requirements);
    updateAgentArg.setState(AgentState.ready);

    rule.setPredicate(predicate1);
    rule.setTag("rule1");

    Route route = new Route();
    route.setQueueId("queue-id1");
    route.setPriority(0L);
    route.setTimeout(300L);
    rule.getRoutes().add(route);

    route = new Route();
    route.setPriority(6L);
    route.setTimeout(600L);
    rule.getRoutes().add(route);

    route = new Route();
    route.setPriority(10L);
    rule.getRoutes().add(route);

    queue.setRouterId("router-id");
    queue.setId("queue-id1");
    queue.setPredicate(predicate2);

  }

  @After
  public void tearDown() {}

  /**
   * Test of evaluateNewTaskToQueueByPlanRules method, of class CommsRouterEvaluator.
   * 
   * @throws java.lang.Exception
   */
  @Test
  public void testEvaluateNewTaskToQueueByPlanRules() throws Exception {
    System.out.println("evaluateNewTaskToQueueByPlanRules");
    CommsRouterEvaluator instance = new CommsRouterEvaluator();
    Boolean expResult = true;
    Boolean result = instance.evaluateNewTaskToQueueByPlanRules(taskId, createTaskArg, rule);
    assertEquals(expResult, result);
  }

  /**
   * Test of evaluateNewAgentForQueue method, of class CommsRouterEvaluator.
   * 
   * @throws java.lang.Exception
   */
  @Test
  public void testEvaluateNewAgentForQueue() throws Exception {
    System.out.println("evaluateNewAgentForQueue");
    CommsRouterEvaluator instance = new CommsRouterEvaluator();
    Boolean expResult = true;
    Boolean result = instance.evaluateNewAgentForQueue(agentId, createAgentArg, queue);
    assertEquals(expResult, result);
  }

  /**
   * Test of evaluateUpdateAgentForQueue method, of class CommsRouterEvaluator.
   * 
   * @throws java.lang.Exception
   */
  @Test
  public void testEvaluateUpdateAgentForQueue() throws Exception {
    System.out.println("evaluateUpdateAgentForQueue");
    CommsRouterEvaluator instance = new CommsRouterEvaluator();
    Boolean expResult = true;
    Boolean result = instance.evaluateUpdateAgentForQueue(agentId, updateAgentArg, queue);
    assertEquals(expResult, result);
  }

  /**
   * Test of evaluateAgentCapabilitiesForQueue method, of class CommsRouterEvaluator.
   * 
   * @throws java.lang.Exception
   */
  @Test
  public void testEvaluateAgentCapabilitiesForQueue() throws Exception {
    System.out.println("evaluateAgentCapabilitiesForQueue");
    CommsRouterEvaluator instance = new CommsRouterEvaluator();
    Boolean expResult = true;
    Boolean result = instance.evaluateAgentCapabilitiesForQueue(agentId, requirements, queue);
    assertEquals(expResult, result);
  }

  /**
   * Test of evaluatePredicateByAttributes method, of class CommsRouterEvaluator.
   * 
   * @throws java.lang.Exception
   */
  @Test
  public void testEvaluatePredicateByAttributes() throws Exception {
    System.out.println("evaluatePredicateByAttributes");
    CommsRouterEvaluator instance = new CommsRouterEvaluator();
    Boolean expResult = false;
    Boolean result = instance.evaluatePredicateByAttributes(requirements, predicate3);
    assertEquals(expResult, result);
  }

}
