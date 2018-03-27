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

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;

import com.softavail.commsrouter.api.exception.ExpressionException;
import com.softavail.commsrouter.domain.Attribute;
import com.softavail.commsrouter.domain.AttributeGroup;
import net.sourceforge.jeval.EvaluationConstants;
import net.sourceforge.jeval.EvaluationException;
import net.sourceforge.jeval.EvaluationResult;
import net.sourceforge.jeval.Evaluator;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Iterator;
import java.util.List;



/**
 *
 * @author Ergyun Syuleyman
 */
public class ExpressionEvaluator extends Evaluator {

  private static final Logger LOGGER = LogManager.getLogger(ExpressionEvaluator.class);

  private boolean isValidation = false;
  private String predicate;
  private String predicateOrigin;

  @Override
  public String replaceVariables(final String expression) throws EvaluationException {
    String replacedVariable = EvaluatorHelpers.resolveBooleanVariable(expression);
    if (replacedVariable != null) {
      return replacedVariable;
    }

    if (isValidation()) {
      replacedVariable = EvaluatorHelpers.validationReplaceIfSingleVariable(expression,
          EvaluationConstants.BOOLEAN_STRING_TRUE);
      if (replacedVariable != null) {
        return replacedVariable;
      }

      replacedVariable = EvaluatorHelpers.validationReplaceIfStringValue(expression,
          EvaluationConstants.BOOLEAN_STRING_TRUE);
      if (replacedVariable != null) {
        return replacedVariable;
      }

      EvaluatorHelpers.validationCheckSpecialCharsInVariable(expression);
    }

    return super.replaceVariables(expression);
  }

  public void init(String predicate) {
    setPredicate(predicate);
    putFunction(new HasFunction(this));
    putFunction(new InFunction(this));
    putFunction(new ContainsFunction());
    setVariableResolver(new CommsRouterVariableResolver(this));
  }

  public void setPredicate(String predicate) {
    this.predicateOrigin = predicate;
    if (predicate != null) {
      this.predicate = EvaluatorHelpers.supportArraysInExpression(predicate);
    }
  }
  
  public String getPredicate() {
    return predicateOrigin;
  }


  public boolean isValidation() {
    return isValidation;
  }

  protected void setIsValidation(boolean isValidation) {
    this.isValidation = isValidation;
  }

  public void validateImpl() throws ExpressionException {
    if (getPredicate() == null || getPredicate().isEmpty()) {
      throw new ExpressionException("Expression cannot be NULL or empty.");
    }
    try {
      this.setIsValidation(true);
      super.evaluate(predicate);
    } catch (EvaluationException ex) {
      throw new ExpressionException("Predicate \"" + getPredicate() + "\" failed with error: "
          + EvaluatorHelpers.getDetailedMessage(ex), ex);
    }
  }

  private String evaluateImpl() throws EvaluationException {
    if (predicateOrigin == null || predicateOrigin.isEmpty()) {
      return EvaluationConstants.BOOLEAN_STRING_FALSE;
    }
    
    this.setIsValidation(false);
    return super.evaluate(predicate);
  }

  public boolean evaluate(AttributeGroup attributes) {
    try {
      setAttributes(attributes);
      String result = evaluateImpl();
      EvaluationResult res = new EvaluationResult(result, EvaluationConstants.SINGLE_QUOTE);
      return !res.isBooleanFalse();
    } catch (EvaluationException ex) {
      String throwException = "Evaluator expression failed with message: " + ex.getMessage();
      LOGGER.info(throwException);
    }
    return false;
  }

  private void setAttributes(AttributeGroup attributesGroup)
      throws RuntimeException {

    clearVariables();

    if (attributesGroup == null) {
      LOGGER.warn("Missing attributes for matching to predicate");
      return;
    }
    List<Attribute> attributes = attributesGroup.getAttributes();
    if (attributes.isEmpty()) {
      LOGGER.warn("Missing attributes for matching to predicate");
      return;
    }

    ListMultimap<String, Object> attributesMap = ArrayListMultimap.create();
    attributes.forEach(jpaAttribute -> {
      String name = jpaAttribute.getName();
      Attribute.Type valueType = jpaAttribute.getType();
      switch (valueType) {
        case STRING:
          if (jpaAttribute.isScalar()) {
            putVariable(name, String.format("'%s'", jpaAttribute.getStringValue()));
          } else {
            attributesMap.put(name, jpaAttribute.getStringValue());
          }
          break;
        case DOUBLE:
          if (jpaAttribute.isScalar()) {
            putVariable(name, jpaAttribute.getDoubleValue().toString());
          } else {
            attributesMap.put(name, jpaAttribute.getDoubleValue());
          }
          break;
        case BOOLEAN:
          if (jpaAttribute.isScalar()) {
            putVariable(name,
                jpaAttribute.getBooleanValue() ? EvaluationConstants.BOOLEAN_STRING_TRUE
                    : EvaluationConstants.BOOLEAN_STRING_FALSE);
          } else {
            throw new RuntimeException(
                "Evaluator: Unexpected array of booleans for attribute" + name);
          }
          break;
        default:
          LOGGER.error("Unexpected attribute value type={}, name={}", valueType,
              jpaAttribute.getName());
          break;
      }
    });

    attributesMap.asMap().forEach((key, value) -> {
      Iterator<Object> iterator = value.iterator();
      String strValue = EvaluatorHelpers.openBracketCharacter;
      boolean firstItem = true;
      while (iterator.hasNext()) {
        if (!firstItem) {
          strValue += EvaluatorHelpers.ARRAY_ITEMS_DELIMITER;
        } else {
          firstItem = false;
        }
        strValue += iterator.next();
      }
      strValue += EvaluatorHelpers.closeBracketCharacter;
      putVariable(key, String.format("'%s'", strValue));
    });
  }

}
