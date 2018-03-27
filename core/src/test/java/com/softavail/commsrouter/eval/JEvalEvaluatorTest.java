/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import com.softavail.commsrouter.api.exception.ExpressionException;
import com.softavail.commsrouter.domain.AttributeGroup;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Ergyun Syuleyman
 */
public class JEvalEvaluatorTest {

  AttributeGroup requirements;
  String predicateOK1;
  String predicateFailed1;
  String predicateOK2;
  String predicateFailed2;
  String predicateOK3;
  String predicateFailed3;
  EvaluatorHelpers evalHelper;

  public JEvalEvaluatorTest() {
    requirements = new AttributeGroup();
    predicateOK1 =
        "#{language} == 'en' && IN(50, #{prices}) && #{price} > 10 && #{boolTrue} == true";
    predicateFailed1 = "#{language} == 'en' && !#{boolFalse} && #{price} > 100";
    predicateOK2 = "(IN('fr',#{languages}) || #{color}=='red') && HAS(#{prices}, 30)";
    predicateFailed2 = "(IN('de',#{languages}) || #{color}=='red') && HAS(#{prices}, 90)";
    predicateOK3 =
        "CONTAINS(#{nickname}, 'Sto') && HAS(#{languages}, 'fr') && IN('en', #{languages}) && #{color}=='red'";
    predicateFailed3 = "CONTAINS(#{nickname}, 'Sto') && HAS(#{languages}, 'de') && #{color}=='red'";
    evalHelper = new EvaluatorHelpers();
  }

  @BeforeClass
  public static void setUpClass() {}

  @AfterClass
  public static void tearDownClass() {}

  @Before
  public void setUp() {
    requirements.add("language", "en");
    requirements.add("nickname", "The Stone");
    requirements.add("color", "red");
    requirements.add("price", 42D);
    requirements.add("boolTrue", true);
    requirements.add("boolFalse", false);

    requirements.addArrayItem("languages", "en");
    requirements.addArrayItem("languages", "es");
    requirements.addArrayItem("languages", "fr");

    requirements.addArrayItem("prices", 20D);
    requirements.addArrayItem("prices", 30D);
    requirements.addArrayItem("prices", 50D);
  }

  @After
  public void tearDown() {}

  /**
   * Test of validate method, of class CommsRouterEvaluator.
   *
   * @throws java.lang.Exception
   */
  @Test
  public void testValidate() throws Exception {
    System.out.println("evaluate");
    CommsRouterEvaluatorFactory ef = new CommsRouterEvaluatorFactory();

    try {
      ef.provide("HAS(#{allowedBools}, true) && IN(true, #{allowedBools}) && #{~bool}", null).validate();
      assertTrue(false);
    } catch (ExpressionException ex) {
    }

    try {
      ef.provide("#{boolTrue} && #{price}>10 && #{$price}^10", null).validate();
      assertTrue(false);
    } catch (ExpressionException ex) {
    }

    try {
      ef.provide("#{color}$'red'", null).validate();
      assertTrue(false);
    } catch (ExpressionException ex) {
    }

    // validation should be passed
    try {
      ef.provide("true", null).validate();
    } catch (ExpressionException ex) {
      assertTrue(false);
    }

    try {
      ef.provide(predicateOK3, null).validate();
    } catch (ExpressionException ex) {
      assertTrue(false);
    }
    try {
      ef.provide("CONTAINS([10, 20, 30], 20)", null).validate();
    } catch (ExpressionException ex) {
      assertTrue(false);
    }
    try {
      ef.provide("IN('fr', ['en','fr'])", null).validate();
    } catch (ExpressionException ex) {
      assertTrue(false);
    }
  }
  
  
  /**
   * Test of evaluateJpa method, of class CommsRouterEvaluator.
   * 
   * @throws java.lang.Exception
   */
  @Test
  public void testEvaluateJpa() throws Exception {
    System.out.println("evaluateJpa");

    CommsRouterEvaluatorFactory ef = new CommsRouterEvaluatorFactory();
    ef.setRsqlValidator(new RsqlDummyValidator());

    JEvalEvaluator instance = new JEvalEvaluator(ef, null);

    Boolean expResult = true;
    Boolean result = instance.changeExpression(predicateOK1, null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.changeExpression(predicateOK2, null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.changeExpression(predicateOK3, null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.changeExpression("1==1", null).evaluate(null);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.changeExpression("HAS(#{language}, 'en')", null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression("2==3", null).evaluate(new AttributeGroup());
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression(null, null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression(predicateFailed1, null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression(predicateFailed2, null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression("CONTAINS('Sto')", null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression("CONTAINS(#{nickname}, #Stone)", null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression("HAS(100)", null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression("HAS([false, 'true'], #{true}) && #{'true'}", null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    requirements.add("departments", "sales; support");
    result = instance.changeExpression("HAS(#{departments}, 'sales')", null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression("HAS(#{languages}, 100)", null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression("IN(#{language}, 'en]')", null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression("IN(200, #{languages})", null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression("IN(50)", null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression("IN(#{false}, [true, 'true']) && #{'false'}", null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.changeExpression(predicateFailed3, null).evaluate(requirements);
    assertEquals(expResult, result);

    // validation should be failed cases
    try {
      requirements.addArrayItem("allowedBools", true);
      requirements.addArrayItem("allowedBools", false);
      instance.changeExpression(predicateOK1, null).evaluate(requirements);
      assertTrue(false);
    } catch (ExpressionException | RuntimeException ex) {
    }

  }

}
