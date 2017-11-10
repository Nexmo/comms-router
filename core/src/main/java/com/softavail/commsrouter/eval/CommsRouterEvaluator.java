/* 
 * Copyright 2017 SoftAvail Inc.
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

import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfDoublesAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfStringsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueVisitor;
import com.softavail.commsrouter.api.dto.model.attribute.BooleanAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.EvaluatorException;
import net.sourceforge.jeval.EvaluationConstants;
import net.sourceforge.jeval.EvaluationException;
import net.sourceforge.jeval.EvaluationResult;
import net.sourceforge.jeval.Evaluator;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.util.Set;

/**
 *
 * @author Ergyun Syuleyman
 */
public class CommsRouterEvaluator {

  private static final Logger LOGGER = LogManager.getLogger(CommsRouterEvaluator.class);


  /**
   *
   * @param expression argument that will be check for valid expression or not
   * @throws EvaluatorException .
   */
  public void isValidExpression(String expression) throws EvaluatorException {
    if (expression == null || expression.isEmpty()) {
      throw new EvaluatorException("Expression cannot be NULL or empty.");
    }

    ExpressionEvaluator evaluator = new ExpressionEvaluator();
    evaluator.init(true);
    evaluator.isValidExpression(expression);
  }

  /**
   *
   * @param attributesGroup agent capabilities arguments for evaluate to queue predicate
   * @param predicate the predicate that will be evaluated
   * @return true - if matched queue
   * @throws CommsRouterException .
   */
  public Boolean evaluate(AttributeGroupDto attributesGroup, String predicate)
      throws CommsRouterException {
    if (predicate == null || predicate.isEmpty()) {
      return false;
    }

    ExpressionEvaluator evaluator = new ExpressionEvaluator();
    evaluator.init();

    return evaluatePredicateToAttributes(evaluator, attributesGroup, predicate);
  }

  private void setEvaluatorAttributeVariables(Evaluator evaluator,
      AttributeGroupDto attributesGroup) {
    if (attributesGroup == null) {
      LOGGER.warn("Missing attributes for matching to predicate");
      return;
    }
    Set<String> keys = attributesGroup.keySet();
    if (keys.isEmpty()) {
      LOGGER.warn("Missing attributes for matching to predicate");
      return;
    }
    keys.forEach((String key) -> {
      AttributeValueDto attributeValue = attributesGroup.get(key);
      try {
        attributeValue.accept(new AttributeValueVisitor() {
          @Override
          public void handleBooleanValue(BooleanAttributeValueDto value) throws IOException {
            evaluator.putVariable(key, value.getValue() ? EvaluationConstants.BOOLEAN_STRING_TRUE
                : EvaluationConstants.BOOLEAN_STRING_FALSE);
          }

          @Override
          public void handleDoubleValue(DoubleAttributeValueDto value) throws IOException {
            evaluator.putVariable(key, String.format("%f", (double) value.getValue()));
          }

          @Override
          public void handleStringValue(StringAttributeValueDto value) throws IOException {
            evaluator.putVariable(key, String.format("'%s'", value.getValue()));
          }

          @Override
          public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value)
              throws IOException {
            evaluator.putVariable(key, String.format("'%s'",
                value.getValue().toString().replace(',', EvaluatorHelpers.ARRAY_ITEMS_DELIMITER)));
          }

          @Override
          public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
              throws IOException {
            evaluator.putVariable(key, String.format("'%s'",
                value.getValue().toString().replace(',', EvaluatorHelpers.ARRAY_ITEMS_DELIMITER)));
          }
        });

      } catch (IOException ex) {
        // this exception will never happens in the block above.
        LOGGER.error("Not expected exception here: {}", ex);
      }
    });
  }

  private Boolean evaluatePredicateToAttributes(Evaluator evaluator,
      AttributeGroupDto attributesGroup, String predicate) throws CommsRouterException {

    setEvaluatorAttributeVariables(evaluator, attributesGroup);

    try {
      String result = evaluator.evaluate(validateExpressionFormat(attributesGroup, predicate));
      EvaluationResult res = new EvaluationResult(result, EvaluationConstants.SINGLE_QUOTE);
      if (res.isBooleanFalse()) {
        return false;
      }

      LOGGER.info("Attribute={} matched to predicate={}", attributesGroup, predicate);
      return true;
    } catch (EvaluationException ex) {
      String throwException = "Evaluator expression failed with message: " + ex.getMessage();
      LOGGER.info(throwException);
      throw new EvaluatorException(throwException, ex);
    }
  }

  private String validateExpressionFormat(AttributeGroupDto attributesGroup, String expression) {
    String formatedExpression = EvaluatorHelpers.supportArraysInExpression(expression);

    return formatedExpression;
  }

}
