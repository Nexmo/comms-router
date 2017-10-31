/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfBooleansAttributeValueDto;
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
import com.softavail.commsrouter.domain.Attribute;
import com.softavail.commsrouter.domain.AttributeGroup;
import com.softavail.commsrouter.domain.dto.mappers.AttributesMapper;
import net.sourceforge.jeval.EvaluationConstants;
import net.sourceforge.jeval.EvaluationException;
import net.sourceforge.jeval.EvaluationResult;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 *
 * @author ergyunsyuleyman
 */
public class CommsRouterEvaluator {

    
   private ExpressionEvaluator evaluator;
   private ExpressionEvaluator validationEvaluator;

   private static final Logger LOGGER = LogManager.getLogger(CommsRouterEvaluator.class);

  public CommsRouterEvaluator initEvaluator(String predicate) {
    if (evaluator == null) {
      evaluator = new ExpressionEvaluator();
      evaluator.init(predicate);
    } else {
      evaluator.setPredicate(predicate);
    }

    return this;
  }

  protected CommsRouterEvaluator initValidatorEvaluator(String predicate) {
    if (validationEvaluator == null) {
      validationEvaluator = new ExpressionEvaluator();
      validationEvaluator.init(predicate, true);
    } else {
      validationEvaluator.setPredicate(predicate);
    }

    return this;
  }

  /**
   *
   * @param expression argument that will be check for valid expression or not
   * @throws EvaluatorException .
   */
  public void isValidExpression(String expression) throws EvaluatorException {
    if (expression == null || expression.isEmpty()) {
      throw new EvaluatorException("Expression cannot be NULL or empty.");
    }

    initValidatorEvaluator(expression);
    validationEvaluator.isValid();
  }

  /**
   *
   * @param attributesGroup evaluator variable attributes using on predicate expression evaluate
   * @return true - if match success
   * @throws CommsRouterException .
   */

  public Boolean evaluate(AttributeGroupDto attributesGroup) throws CommsRouterException {
    if (evaluator == null) {
      throw new EvaluatorException("Predicate evaluator is not initialized with expression value. "
                  + "Please call 'initEvaluator(String predicate)' first.");
    }
    setEvaluatorAttributeVariables(attributesGroup);
    if (evaluatePredicateToAttributes()) {
      LOGGER.info("Attribute={} matched to predicate={}", attributesGroup,
          evaluator.getPredicate());
      return true;
    }

    return false;
  }


  /**
   *
   * @param attributesGroup evaluator variable attributes using on predicate expression evaluate
   * @return true - if match success
   * @throws CommsRouterException .
   */
  public Boolean evaluateJpa(AttributeGroup attributesGroup) throws CommsRouterException {
    if (evaluator == null) {
      throw new EvaluatorException("Predicate evaluator is not initialized with expression value. "
          + "Please call 'initEvaluator(String predicate)' first.");
    }
    setEvaluatorJpaAttributeVariables(attributesGroup);
    if (evaluatePredicateToAttributes()) {
      LOGGER.info("Attribute={} matched to predicate={}", attributesGroup,
          evaluator.getPredicate());
      return true;
    }
    
    return false;
  }

  private void setEvaluatorAttributeVariables(AttributeGroupDto attributesGroup) {
    evaluator.clearVariables();
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

          @Override
          public void handleArrayOfBooleansValue(ArrayOfBooleansAttributeValueDto value)
              throws IOException {
            evaluator.putVariable(key, String.format("'%s'",
                value.getValue().toString().replace(',', EvaluatorHelpers.ARRAY_ITEMS_DELIMITER)));
          }
        });

      } catch (IOException ex) {
        // this exception shouldn't happen in the block above.
        LOGGER.error("Unexpected exception here: {}", ex);
      }
    });
  }

  private void setEvaluatorJpaAttributeVariables(AttributeGroup attributesGroup) {
    evaluator.clearVariables();
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
      AttributesMapper.JpaAttributeValueType valueType = AttributesMapper.getJpaAttributeValueType(jpaAttribute);
      switch (valueType) {
        case STRING:
          attributesMap.put(name, jpaAttribute.getStringValue());
          break;
        case DOUBLE:
          attributesMap.put(name, jpaAttribute.getDoubleValue());
          break;
        case BOOLEAN:
          attributesMap.put(name, jpaAttribute.getBooleanValue());
          break;
        default:
          LOGGER.error("Unexpected attribute value type={}, name={}", valueType, jpaAttribute.getName());
          break;
      }
    });

    attributesMap.asMap().forEach((key, value) -> {
      if (value.size() == 1) {
        Object variable = value.iterator().next();
        if (variable instanceof Double) {
          evaluator.putVariable(key, ((Double) variable).toString());
        } else if (variable instanceof Boolean) {
          evaluator.putVariable(key, (Boolean) variable ? EvaluationConstants.BOOLEAN_STRING_TRUE
                : EvaluationConstants.BOOLEAN_STRING_FALSE);
        } else {
          evaluator.putVariable(key, String.format("'%s'", variable));
        }
      } else {
        // value collections in this multimap view are always non-empty, so here we have more than
        // one element
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
        evaluator.putVariable(key, String.format("'%s'", strValue));
      }
    });
  }

  private Boolean evaluatePredicateToAttributes()
      throws CommsRouterException {

    try {
      String result = evaluator.evaluateImpl();
      EvaluationResult res = new EvaluationResult(result, EvaluationConstants.SINGLE_QUOTE);
      return !res.isBooleanFalse();
    } catch (EvaluationException ex) {
      String throwException = "Evaluator expression failed with message: " + ex.getMessage();
      LOGGER.info(throwException);
      throw new EvaluatorException(throwException, ex);
    }
  }

}
