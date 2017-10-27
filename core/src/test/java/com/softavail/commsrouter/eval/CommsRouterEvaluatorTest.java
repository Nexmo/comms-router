/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package com.softavail.commsrouter.eval;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfBooleansAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfDoublesAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfStringsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.BooleanAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.exception.EvaluatorException;
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
  String predicateOK1;
  String predicateFailed1;
  String predicateOK2;
  String predicateFailed2;
  String predicateOK3;
  String predicateFailed3;
  EvaluatorHelpers evalHelper;

  public CommsRouterEvaluatorTest() {
    createTaskArg = new CreateTaskArg();
    createAgentArg = new CreateAgentArg();
    updateAgentArg = new UpdateAgentArg();
    requirements = new AttributeGroupDto();

    rule = new Rule();
    queue = new Queue();
    taskId = "task-id1";
    agentId = "agent-id1";
    predicateOK1 =
        "#{language} == 'en' && IN(50, #{prices}) && #{price} > 10 && #{boolTrue} == true";
    predicateFailed1 = "#{language} == 'en' && !#{boolFalse} && #{price} > 100";
    predicateOK2 = "(IN('fr',#{languages}) || #{color}=='red') && HAS(#{prices}, 30)";
    predicateFailed2 = "(IN('fr',['en','es','de']) || #{color}=='red') && HAS(#{prices}, 90)";
    predicateOK3 = "CONTAINS(#{nickname}, 'Sto') && HAS(#{languages}, 'fr') && #{color}=='red'";
    predicateFailed3 = "CONTAINS(#{nickname}, 'Sto') && HAS(#{languages}, 'de') && #{color}=='red'";
    evalHelper = new EvaluatorHelpers();
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
    requirements.put("boolTrue", new BooleanAttributeValueDto(true));
    requirements.put("boolFalse", new BooleanAttributeValueDto(false));
    ArrayOfStringsAttributeValueDto languages = new ArrayOfStringsAttributeValueDto();
    languages.add("en");
    languages.add("es");
    languages.add("fr");
    requirements.put("languages", languages);
    ArrayOfDoublesAttributeValueDto prices = new ArrayOfDoublesAttributeValueDto();
    prices.add(20D);
    prices.add(30D);
    prices.add(50D);
    requirements.put("prices", prices);
    ArrayOfBooleansAttributeValueDto allowedBools = new ArrayOfBooleansAttributeValueDto();
    allowedBools.add(true);
    allowedBools.add(false);
    requirements.put("allowedBools", allowedBools);
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

    rule.setPredicate(predicateOK1);
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
    queue.setPredicate(predicateOK2);

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
    rule.setPredicate(predicateOK1);
    Boolean result = instance.evaluateNewTaskToQueueByPlanRules(taskId, createTaskArg, rule);
    assertEquals(expResult, result);
    expResult = false;
    rule.setPredicate(predicateFailed1);
    result = instance.evaluateNewTaskToQueueByPlanRules(taskId, createTaskArg, rule);
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
    queue.setPredicate(predicateOK2);
    Boolean result = instance.evaluateAgentCapabilitiesForQueue(agentId, requirements, queue);
    assertEquals(expResult, result);
    expResult = false;
    queue.setPredicate(predicateFailed2);
    result = instance.evaluateAgentCapabilitiesForQueue(agentId, requirements, queue);
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
    Boolean expResult = true;
    Boolean result = true;
    // validation should be failed cases
    try {
      instance.isValidExpression(null);
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }

    try {
      instance
          .isValidExpression("HAS(#{allowedBools}, true) && IN(true, #{allowedBools}) && #{~bool}");
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }

    try {
      instance.isValidExpression("#{boolTrue} && #{price}>10 && #{$price}^10");
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }

    try {
      instance.isValidExpression("#{color}$'red'");
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }

    // validation should be passed
    try {
      instance.isValidExpression("true");
    } catch (EvaluatorException ex) {
      assertTrue(false);
    }

    try {
      instance.isValidExpression(predicateOK3);
    } catch (EvaluatorException ex) {
      assertTrue(false);
    }
    try {
      instance.isValidExpression("CONTAINS([10, 20, 30], 20)");
    } catch (EvaluatorException ex) {
      assertTrue(false);
    }
    try {
      instance.isValidExpression("IN('fr', ['en','fr'])");
    } catch (EvaluatorException ex) {
      assertTrue(false);
    }

    // check expressions by attributte
    expResult = true;
    result = instance.evaluatePredicateByAttributes(requirements, predicateOK3);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.evaluatePredicateByAttributes(null, "1==1");
    assertEquals(expResult, result);
    expResult = false;
    result = instance.evaluatePredicateByAttributes(new AttributeGroupDto(), "2==3");
    assertEquals(expResult, result);
    expResult = false;
    result = instance.evaluatePredicateByAttributes(requirements, null);
    assertEquals(expResult, result);
    
    try {
      instance.evaluatePredicateByAttributes(requirements, "CONTAINS('Sto')");
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }
    
    try {
      instance.evaluatePredicateByAttributes(requirements, "HAS(100)");
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }
    try {
      instance.evaluatePredicateByAttributes(requirements,
          "HAS([false, 'true'], #{true}) && #{'true'}");
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }
    try {
      instance.evaluatePredicateByAttributes(requirements, "IN(50)");
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }
    try {
      instance.evaluatePredicateByAttributes(requirements,
          "IN(#{false}, [true, 'true']) && #{'false'}");
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }

    expResult = false;
    result = instance.evaluatePredicateByAttributes(requirements, predicateFailed3);
    assertEquals(expResult, result);
  }

}
