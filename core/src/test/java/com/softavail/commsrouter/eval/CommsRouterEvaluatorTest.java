/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfDoublesAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfStringsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.BooleanAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.exception.EvaluatorException;
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
public class CommsRouterEvaluatorTest {

  AttributeGroupDto requirements;
  AttributeGroup requirementsJpa;
  String predicateOK1;
  String predicateFailed1;
  String predicateOK2;
  String predicateFailed2;
  String predicateOK3;
  String predicateFailed3;
  EvaluatorHelpers evalHelper;

  public CommsRouterEvaluatorTest() {
    requirements = new AttributeGroupDto();
    requirementsJpa = new AttributeGroup();
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

    requirementsJpa.add("language", "en");
    requirementsJpa.add("nickname", "The Stone");
    requirementsJpa.add("color", "red");
    requirementsJpa.add("price", 42D);
    requirementsJpa.add("boolTrue", true);
    requirementsJpa.add("boolFalse", false);

    requirementsJpa.addArrayItem("languages", "en");
    requirementsJpa.addArrayItem("languages", "es");
    requirementsJpa.addArrayItem("languages", "fr");

    requirementsJpa.addArrayItem("prices", 20D);
    requirementsJpa.addArrayItem("prices", 30D);
    requirementsJpa.addArrayItem("prices", 50D);
  }

  @After
  public void tearDown() {}

  /**
   * Test of evaluate method, of class CommsRouterEvaluator.
   *
   * @throws java.lang.Exception
   */
  @Test
  public void testEvaluate() throws Exception {
    System.out.println("evaluate");
    CommsRouterEvaluator instance = new CommsRouterEvaluator();

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
    Boolean expResult = true;
    Boolean result = instance.initEvaluator(predicateOK1).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.initEvaluator(predicateOK2).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.initEvaluator(predicateOK3).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.initEvaluator("1==1").evaluate(null);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.initEvaluator("HAS(#{language}, 'en')").evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.initEvaluator("2==3").evaluate(new AttributeGroupDto());
    assertEquals(expResult, result);
    expResult = false;
    result = instance.initEvaluator(null).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.initEvaluator(predicateFailed1).evaluate(requirements);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.initEvaluator(predicateFailed2).evaluate(requirements);
    assertEquals(expResult, result);

    try {
      instance.initEvaluator("CONTAINS('Sto')").evaluate(requirements);
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }

    try {
      instance.initEvaluator("CONTAINS(#{nickname}, #Stone)").evaluate(requirements);
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }

    try {
      instance.initEvaluator("HAS(100)").evaluate(requirements);
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }
    try {
      instance.initEvaluator("HAS([false, 'true'], #{true}) && #{'true'}").evaluate(requirements);
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }
    try {
      requirements.put("departments", new StringAttributeValueDto("sales; support"));
      instance.initEvaluator("HAS(#{departments}, 'sales')").evaluate(requirements);
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }
    try {
      instance.initEvaluator("HAS(#{languages}, 100)").evaluate(requirements);
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }
    try {
      instance.initEvaluator("IN(#{language}, 'en]')").evaluate(requirements);
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }
    try {
      instance.initEvaluator("IN(200, #{languages})").evaluate(requirements);
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }
    try {
      instance.initEvaluator("IN(50)").evaluate(requirements);
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }
    try {
      instance.initEvaluator("IN(#{false}, [true, 'true']) && #{'false'}").evaluate(requirements);
      assertTrue(false);
    } catch (EvaluatorException ex) {
    }

    expResult = false;
    result = instance.initEvaluator(predicateFailed3).evaluate(requirements);
    assertEquals(expResult, result);
  }

  
  
  /**
   * Test of evaluateJpa method, of class CommsRouterEvaluator.
   * 
   * @throws java.lang.Exception
   */
  @Test
  public void testEvaluateJpa() throws Exception {
    System.out.println("evaluateJpa");
    CommsRouterEvaluator instance = new CommsRouterEvaluator();
    Boolean expResult = true;
    Boolean result = instance.initEvaluator(predicateOK1).evaluateJpa(requirementsJpa);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.initEvaluator(predicateOK2).evaluateJpa(requirementsJpa);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.initEvaluator(predicateOK3).evaluateJpa(requirementsJpa);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.initEvaluator("1==1").evaluateJpa(null);
    assertEquals(expResult, result);
    expResult = true;
    result = instance.initEvaluator("HAS(#{language}, 'en')").evaluateJpa(requirementsJpa);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.initEvaluator("2==3").evaluateJpa(new AttributeGroup());
    assertEquals(expResult, result);
    expResult = false;
    result = instance.initEvaluator(null).evaluateJpa(requirementsJpa);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.initEvaluator(predicateFailed1).evaluateJpa(requirementsJpa);
    assertEquals(expResult, result);
    expResult = false;
    result = instance.initEvaluator(predicateFailed2).evaluateJpa(requirementsJpa);
    assertEquals(expResult, result);

    // validation should be failed cases
    try {
      requirementsJpa.addArrayItem("allowedBools", true);
      requirementsJpa.addArrayItem("allowedBools", false);
      instance.initEvaluator(predicateOK1).evaluateJpa(requirementsJpa);
      assertTrue(false);
    } catch (EvaluatorException | RuntimeException ex) {
    }

  }

}
