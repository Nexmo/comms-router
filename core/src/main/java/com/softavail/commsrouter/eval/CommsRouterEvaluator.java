/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
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
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Rule;
import net.sourceforge.jeval.EvaluationConstants;
import net.sourceforge.jeval.EvaluationException;
import net.sourceforge.jeval.EvaluationResult;
import net.sourceforge.jeval.Evaluator;
import net.sourceforge.jeval.VariableResolver;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.util.Set;

/**
 *
 * @author ergyunsyuleyman
 */
public class CommsRouterEvaluator {

  private static final Logger LOGGER = LogManager.getLogger(CommsRouterEvaluator.class);
  // private static final String EVAL_VARIABLES_FORMAT = "#{%s}";
  private final int openBracketCharacter = '[';
  private final int closeBracketCharacter = ']';


  public static class VariableResolverEx implements VariableResolver {

    @Override
    public String resolveVariable(String variableName) {
      return EvaluatorHelpers.resolveBooleanVariable(variableName);
    }
  }

  public static class EvaluatorEx extends Evaluator {

    @Override
    public String replaceVariables(final String expression) throws EvaluationException {
      String replacedVariable = EvaluatorHelpers.resolveBooleanVariable(expression);
      if (replacedVariable != null) {
        return replacedVariable;
      }

      return super.replaceVariables(expression);
    }
  }



  /**
   * 
   * @param taskId new creating task ID
   * @param createTaskArg new task arguments
   * @param rule CommsRouter plan rule object
   * @return matched queue ID for the matched RuleDto OR null if not match
   * @throws com.softavail.commsrouter.api.exception.CommsRouterException
   *
   */
  public Boolean evaluateNewTaskToQueueByPlanRules(String taskId, CreateTaskArg createTaskArg,
      Rule rule) throws CommsRouterException {
    AttributeGroupDto attrbutes = createTaskArg.getRequirements();
    if (evaluatePredicateByAttributes(attrbutes, rule.getPredicate())) {
      LOGGER.info("The task with ID={} matched to rule predicate for queue with ID={}", taskId,
          rule.getQueueId());
      return true;
    }

    return false;
  }

  /**
   *
   * @param agentId new creating agent ID
   * @param createAgentArg arguments for creating an agent
   * @param queue the queue that will be evaluated
   * @return true - if matched queue
   * @throws CommsRouterException .
   */
  public Boolean evaluateNewAgentForQueue(String agentId, CreateAgentArg createAgentArg,
      Queue queue) throws CommsRouterException {
    AttributeGroupDto attrbutes = createAgentArg.getCapabilities();
    if (evaluatePredicateByAttributes(attrbutes, queue.getPredicate())) {
      LOGGER.info("The agent with ID={} matched to queue with ID={}", agentId, queue.getId());
      return true;
    }

    return false;
  }

  /**
   *
   * @param agentId updating agent ID
   * @param updateAgentArg arguments for updating agent
   * @param queue the queue that will be evaluated
   * @return true - if matched queue
   * @throws CommsRouterException .
   */
  public Boolean evaluateUpdateAgentForQueue(String agentId, UpdateAgentArg updateAgentArg,
      Queue queue) throws CommsRouterException {
    AttributeGroupDto attrbutes = updateAgentArg.getCapabilities();
    if (evaluatePredicateByAttributes(attrbutes, queue.getPredicate())) {
      LOGGER.info("The updated agent with ID={} matched to queue with ID={}", agentId,
          queue.getId());
      return true;
    }

    return false;
  }

  /**
   *
   * @param agentId agent ID for which capabilities will be evaluated to queue
   * @param agentAttrbutes agent capabilities arguments for evaluate to queue predicate
   * @param queue the queue that will be evaluated
   * @return true - if matched queue
   * @throws CommsRouterException .
   */
  public Boolean evaluateAgentCapabilitiesForQueue(String agentId, AttributeGroupDto agentAttrbutes,
      Queue queue) throws CommsRouterException {
    if (evaluatePredicateByAttributes(agentAttrbutes, queue.getPredicate())) {
      LOGGER.info("The agent with ID={} matched to queue with ID={}", agentId, queue.getId());
      return true;
    }

    return false;
  }

  private Boolean evaluatePredicateByAttributes(AttributeGroupDto attributesGroup, String pridicate)
      throws CommsRouterException {
    if (pridicate == null || pridicate.isEmpty()) {
      return false;
    }
    Evaluator evaluator = new EvaluatorEx();
    evaluator.putFunction(new HasFunction());
    evaluator.putFunction(new InFunction());
    evaluator.putFunction(new ContainsFunction());
    evaluator.setVariableResolver(new VariableResolverEx());

    return evaluatePredicateToAttributes(evaluator, attributesGroup, pridicate);
  }

  private Boolean setEvaluatorAttributeVariables(Evaluator evaluator,
      AttributeGroupDto attributesGroup) {
    Boolean result = true;
    if (attributesGroup == null) {
      LOGGER.warn("Missing attributes for matching to predicate");
      return true;
    }
    Set<String> keys = attributesGroup.keySet();
    if (keys.isEmpty()) {
      LOGGER.warn("Missing attributes for matching to predicate");
      result = true;
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
            evaluator.putVariable(key,
                String.format("'%s'", value.getValue().toString().replace(',', ';')));
          }

          @Override
          public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
              throws IOException {
            evaluator.putVariable(key,
                String.format("'%s'", value.getValue().toString().replace(',', ';')));
          }

          @Override
          public void handleArrayOfBooleansValue(ArrayOfBooleansAttributeValueDto value)
              throws IOException {
            evaluator.putVariable(key,
                String.format("'%s'", value.getValue().toString().replace(',', ';')));
          }
        });

      } catch (IOException ex) {
        LOGGER.error(ex.getLocalizedMessage());
      }
    });

    return result;
  }

  private Boolean evaluatePredicateToAttributes(Evaluator evaluator,
      AttributeGroupDto attributesGroup, String predicate) throws CommsRouterException {

    if (!setEvaluatorAttributeVariables(evaluator, attributesGroup)) {
      return false;
    }

    String throwException;
    try {
      String result = evaluator.evaluate(validateExpressionFormat(attributesGroup, predicate));
      EvaluationResult res = new EvaluationResult(result, EvaluationConstants.SINGLE_QUOTE);
      if (res.isBooleanFalse()) {
        return false;
      }

      LOGGER.info("Attribute={} matched to predicate={}", attributesGroup, predicate);
      return true;
    } catch (EvaluationException ex) {
      LOGGER.info("Evaluator::evaluate() failed with error: '{}", ex.getLocalizedMessage());
      throwException = ex.getLocalizedMessage();
    }

    if (throwException != null && !throwException.isEmpty()) {
      throw new EvaluatorException(throwException);
    }

    return false;
  }

  private String reMapVariableKeysInPredicate(AttributeGroupDto attributesGroup, String predicate) {
    // if (attributesGroup == null) {
    // return predicate;
    // }
    // String result = predicate;
    // Set<String> keys = attributesGroup.keySet();
    // for (String key : keys) {
    // String newKey = String.format(EVAL_VARIABLES_FORMAT, key);
    // result = result.replaceAll(key, newKey);
    // }
    // return result;
    return predicate;
  }

  private String supportArraysInExpression(String expression) {
    String formatedExpression = expression;
    int startIndex = 0;
    do {
      startIndex = formatedExpression.indexOf(openBracketCharacter, startIndex);
      if (startIndex >= 0) {
        int endIndex = formatedExpression.indexOf(closeBracketCharacter, startIndex + 1);
        if (endIndex > 0) {
          String arrayString = formatedExpression.substring(startIndex, endIndex + 1);
          arrayString = String.format("'%s'", arrayString.replace(',', ';'));
          formatedExpression = formatedExpression.substring(0, startIndex) + arrayString
              + formatedExpression.substring(endIndex + 1);
          endIndex += 3;
        }
        startIndex = endIndex;
      }
    } while (startIndex > 0);
    return formatedExpression;
  }

  private String validateExpressionFormat(AttributeGroupDto attributesGroup, String expression) {
    String formatedExpression = supportArraysInExpression(expression);
    formatedExpression = reMapVariableKeysInPredicate(attributesGroup, formatedExpression);

    return formatedExpression;
  }

}
