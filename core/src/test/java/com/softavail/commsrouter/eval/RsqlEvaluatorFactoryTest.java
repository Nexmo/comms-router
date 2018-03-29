/*
 * Copyright 2018 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.commsrouter.eval;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.softavail.commsrouter.api.exception.ExpressionException;
import com.softavail.commsrouter.domain.AttributeGroup;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Vladislav Todorov
 */
public class RsqlEvaluatorFactoryTest {

  RsqlEvaluatorFactory rsqlEvaluatorFactory;
  AttributeGroup attributeGroupe;

  public RsqlEvaluatorFactoryTest() {
    attributeGroupe = new AttributeGroup();
    CommsRouterEvaluatorFactory commsRouterEvaluatorFactory = new CommsRouterEvaluatorFactory();
    commsRouterEvaluatorFactory.setRsqlValidator(new RsqlDummyValidator());
    rsqlEvaluatorFactory = new RsqlEvaluatorFactory(commsRouterEvaluatorFactory);
  }

  @Before
  public void setUp() {

    attributeGroupe.add("language", "en");
    attributeGroupe.add("nickname", "The Stone");
    attributeGroupe.add("color", "red");
    attributeGroupe.add("price", 30d);
    attributeGroupe.add("boolTrue", true);
    attributeGroupe.add("boolFalse", false);

    attributeGroupe.addArrayItem("languages", "en");
    attributeGroupe.addArrayItem("languages", "es");
    attributeGroupe.addArrayItem("languages", "fr");
    attributeGroupe.addArrayItem("prices", 20D);
    attributeGroupe.addArrayItem("prices", 30D);
    attributeGroupe.addArrayItem("prices", 50D);

  }

  @Test(expected = ExpressionException.class)
  public void comparisionGtMultyArgTest() throws Exception {
    rsqlEvaluatorFactory.create("skill>(1,2,3)", "routerRef");
  }

  @Test(expected = ExpressionException.class)
  public void comparisionGeMultyArgTest() throws Exception {
    rsqlEvaluatorFactory.create("skill>=(1,2,3)", "routerRef");
  }

  @Test(expected = ExpressionException.class)
  public void comparisionLtMultyArgTest() throws Exception {
    rsqlEvaluatorFactory.create("skill<(1,2,3)", "routerRef");
  }

  @Test(expected = ExpressionException.class)
  public void comparisionLeMultyArgTest() throws Exception {
    rsqlEvaluatorFactory.create("skill<=(1,2,3)", "routerRef");
  }

  @Test(expected = ExpressionException.class)
  public void comparisionEqMultyArgTest() throws Exception {
    rsqlEvaluatorFactory.create("skill==(1,2,3)", "routerRef");
  }

  @Test(expected = ExpressionException.class)
  public void comparisionNeMultyArgTest() throws Exception {
    rsqlEvaluatorFactory.create("skill!=(1,2,3)", "routerRef");
  }

  @Test
  public void comparisionAllSingleArgTest() throws Exception {
    rsqlEvaluatorFactory.create("skill>1", "routerRef");
    rsqlEvaluatorFactory.create("skill>=1", "routerRef");
    rsqlEvaluatorFactory.create("skill<1", "routerRef");
    rsqlEvaluatorFactory.create("skill<=1", "routerRef");
    rsqlEvaluatorFactory.create("skill==1", "routerRef");
    rsqlEvaluatorFactory.create("skill!=1", "routerRef");
  }

  @Test
  public void evaluateExpressionTrue() throws Exception {

    String predicateOK1 = "language==en;price=in=(20,30,40);price=gt=10;boolTrue==true";
    String predicateOK2 = "language==bg,price<100;boolFalse==false";
    String predicateOK3 = "language=in=(en,fr,es);prices==30,color==blue";

    assertTrue(rsqlEvaluatorFactory.evaluate(predicateOK1, attributeGroupe, "routerRef"));
    assertTrue(rsqlEvaluatorFactory.evaluate(predicateOK2, attributeGroupe, "routerRef"));
    assertTrue(rsqlEvaluatorFactory.evaluate(predicateOK3, attributeGroupe, "routerRef"));

    assertTrue(rsqlEvaluatorFactory.create(predicateOK1, "routerRef").evaluate(attributeGroupe));
    assertTrue(rsqlEvaluatorFactory.create(predicateOK2, "routerRef").evaluate(attributeGroupe));
    assertTrue(rsqlEvaluatorFactory.create(predicateOK3, "routerRef").evaluate(attributeGroupe));
  }

  @Test
  public void evaluateExpressionFalse() throws Exception {

    String predicateNOK1 = "language==en;price=in=(20,30,40);price=gt=30;boolTrue==true";
    String predicateNOK2 = "language==bg,price<30,boolFalse==true";
    String predicateNOK3 = "language=in=(bg,fr,es);color==red;prices==30";

    assertFalse(rsqlEvaluatorFactory.evaluate(predicateNOK1, attributeGroupe, "routerRef"));
    assertFalse(rsqlEvaluatorFactory.evaluate(predicateNOK2, attributeGroupe, "routerRef"));
    assertFalse(rsqlEvaluatorFactory.evaluate(predicateNOK3, attributeGroupe, "routerRef"));

    assertFalse(rsqlEvaluatorFactory.create(predicateNOK1, "routerRef").evaluate(attributeGroupe));
    assertFalse(rsqlEvaluatorFactory.create(predicateNOK2, "routerRef").evaluate(attributeGroupe));
    assertFalse(rsqlEvaluatorFactory.create(predicateNOK3, "routerRef").evaluate(attributeGroupe));
  }

  @Test(expected = ExpressionException.class)
  public void evaluateExpressionInvalidAttributesNumber() throws Exception {
    String predicate = "languages=gt=en";
    rsqlEvaluatorFactory.evaluate(predicate, attributeGroupe, "routerRef");
  }

  @Test
  public void validateExpressionValid() throws Exception {

    String predicateOK1 = "language==en;price=in=(20,30,40);price=gt=10;boolTrue==true";
    String predicateOK2 = "language==bg,price<100;boolFalse==false";
    String predicateOK3 = "language=in=(en,fr,es);prices==30,color==blue";

    rsqlEvaluatorFactory.validate(predicateOK1);
    rsqlEvaluatorFactory.validate(predicateOK2);
    rsqlEvaluatorFactory.validate(predicateOK3);
  }

  @Test(expected = ExpressionException.class)
  public void validateExpressionInalid1() throws Exception {
    String predicateNOK1 = "language===en";
    rsqlEvaluatorFactory.validate(predicateNOK1);
  }

  @Test(expected = ExpressionException.class)
  public void validateExpressionInalid2() throws Exception {
    String predicateNOK2 = "language==bg:boolFalse==true";
    rsqlEvaluatorFactory.validate(predicateNOK2);
  }

  @Test(expected = ExpressionException.class)
  public void validateExpressionInalid3() throws Exception {
    String predicateNOK3 = "language==in=(bg,fr,es)";
    rsqlEvaluatorFactory.validate(predicateNOK3);
  }
}
