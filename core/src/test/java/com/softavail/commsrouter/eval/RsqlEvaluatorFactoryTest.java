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

import com.softavail.commsrouter.api.exception.EvaluatorException;
import com.softavail.commsrouter.domain.AttributeGroup;
import static junit.framework.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

/**
 *
 * @author Vladislav Todorov
 */
public class RsqlEvaluatorFactoryTest {

    RsqlEvaluatorFactory rsqlEvaluatorFactory;
    AttributeGroup attributeGroupe;

    public RsqlEvaluatorFactoryTest() {
        attributeGroupe = new AttributeGroup();
        rsqlEvaluatorFactory = new RsqlEvaluatorFactory();
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

    @Test
    public void evaluateExpressionTrue() throws Exception {

        String predicateOK1 = "language==en;price=in=(20,30,40);price=gt=10;boolTrue==true";
        String predicateOK2 = "language==bg,price<100;boolFalse==false";
        String predicateOK3 = "language=in=(en,fr,es);prices==30,color==blue";

        assertTrue(rsqlEvaluatorFactory.evaluate(predicateOK1, attributeGroupe));
        assertTrue(rsqlEvaluatorFactory.evaluate(predicateOK2, attributeGroupe));
        assertTrue(rsqlEvaluatorFactory.evaluate(predicateOK3, attributeGroupe));

        assertTrue(rsqlEvaluatorFactory.create(predicateOK1).evaluate(attributeGroupe));
        assertTrue(rsqlEvaluatorFactory.create(predicateOK2).evaluate(attributeGroupe));
        assertTrue(rsqlEvaluatorFactory.create(predicateOK3).evaluate(attributeGroupe));
    }

    @Test
    public void evaluateExpressionFalse() throws Exception {

        String predicateNOK1 = "language==en;price=in=(20,30,40);price=gt=30;boolTrue==true";
        String predicateNOK2 = "language==bg,price<30,boolFalse==true";
        String predicateNOK3 = "language=in=(bg,fr,es);color==red;prices==30";

        assertFalse(rsqlEvaluatorFactory.evaluate(predicateNOK1, attributeGroupe));
        assertFalse(rsqlEvaluatorFactory.evaluate(predicateNOK2, attributeGroupe));
        assertFalse(rsqlEvaluatorFactory.evaluate(predicateNOK3, attributeGroupe));

        assertFalse(rsqlEvaluatorFactory.create(predicateNOK1).evaluate(attributeGroupe));
        assertFalse(rsqlEvaluatorFactory.create(predicateNOK2).evaluate(attributeGroupe));
        assertFalse(rsqlEvaluatorFactory.create(predicateNOK3).evaluate(attributeGroupe));
    }

    @Test(expected = NullPointerException.class)
    public void evaluateExpressionInvalidAttributesNumber() throws Exception {
        String predicate = "languages=gt=en";
        rsqlEvaluatorFactory.evaluate(predicate, attributeGroupe);
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

    @Test(expected = EvaluatorException.class)
    public void validateExpressionInalid1() throws Exception {
        String predicateNOK1 = "language===en";
        rsqlEvaluatorFactory.validate(predicateNOK1);
    }

    @Test(expected = EvaluatorException.class)
    public void validateExpressionInalid2() throws Exception {
        String predicateNOK2 = "language==bg:boolFalse==true";
        rsqlEvaluatorFactory.validate(predicateNOK2);
    }

    @Test(expected = EvaluatorException.class)
    public void validateExpressionInalid3() throws Exception {
        String predicateNOK3 = "language==in=(bg,fr,es)";
        rsqlEvaluatorFactory.validate(predicateNOK3);
    }
}
