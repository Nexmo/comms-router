/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfBooleansAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfLongsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfStringsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueVisitor;
import com.softavail.commsrouter.api.dto.model.attribute.BooleanAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.LongAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.EvaluatorException;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Rule;

import net.sourceforge.jeval.EvaluationConstants;
import net.sourceforge.jeval.EvaluationException;
import net.sourceforge.jeval.EvaluationResult;
import net.sourceforge.jeval.Evaluator;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;

import java.util.List;
import java.util.Set;

/**
 *
 * @author ergyunsyuleyman
 */
public class CommsRouterEvaluator {

  private static final Logger LOGGER = LogManager.getLogger(CommsRouterEvaluator.class);
  private static final String EVAL_VARIABLES_FORMAT = "#{%s}";

  /**
   * 
   * @param createTaskArg new task arguments
   * @param plan CommsRouter plan object
   * @return matched queue ID for the matched RuleDto OR null if not match
   *
   */
  public String evaluateNewTaskToQueueByPlanRules(CreateTaskArg createTaskArg, Plan plan)
      throws CommsRouterException {
    AttributeGroupDto attrbutes = createTaskArg.getRequirements();
    String queueId = evaluatePlanQueueByAttributes(attrbutes, plan);
    if (queueId != null) {
      createTaskArg.setQueueId(queueId);
      LOGGER.info("The task with ID={} matched to queue with ID={}", createTaskArg.getId(),
          queueId);
    }

    return queueId;
  }

  /**
   *
   * @param createAgentArg arguments for creating an agent
   * @param queue the queue that will be evaluated
   * @return true - if matched queue
   */
  public Boolean evaluateNewAgentForQueue(CreateAgentArg createAgentArg, Queue queue)
      throws CommsRouterException {
    AttributeGroupDto attrbutes = createAgentArg.getCapabilities();
    if (evaluatePredicateByAttributes(attrbutes, queue.getPredicate())) {
      LOGGER.info("The agent with ID={} matched to queue with ID={}", createAgentArg.getId(),
          queue.getId());
      return true;
    }

    return false;
  }

  /**
   *
   * @param updateAgentArg arguments for updating agent
   * @param queue the queue that will be evaluated
   * @return true - if matched queue
   */
  public Boolean evaluateUpdateAgentForQueue(UpdateAgentArg updateAgentArg, Queue queue)
      throws CommsRouterException {
    AttributeGroupDto attrbutes = updateAgentArg.getCapabilities();
    if (evaluatePredicateByAttributes(attrbutes, queue.getPredicate())) {
      LOGGER.info("The updated agent with ID={} matched to queue with ID={}",
          updateAgentArg.getId(), queue.getId());
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
   */
  public Boolean evaluateAgentCapabilitiesForQueue(String agentId, AttributeGroupDto agentAttrbutes,
      Queue queue) throws CommsRouterException {
    if (evaluatePredicateByAttributes(agentAttrbutes, queue.getPredicate())) {
      LOGGER.info("The agent with ID={} matched to queue with ID={}", agentId, queue.getId());
      return true;
    }

    return false;
  }

  /**
   *
   * @param attributesGroup attributes group to evaluate some queues
   * @param plan CommsRouter plan object
   * @return matched queue ID for the matched RuleDto OR null if not match
   */
  public String evaluatePlanQueueByAttributes(AttributeGroupDto attributesGroup, Plan plan)
      throws CommsRouterException {
    Evaluator evaluator = new Evaluator();
    evaluator.putFunction(new HasFunction());
    evaluator.putFunction(new InFunction());
    evaluator.putFunction(new ContainsFunction());
    if (!setEvaluatorAttributeVariables(evaluator, attributesGroup)) {
      return null;
    }

    List<Rule> rules = plan.getRules();
    String queueId = null;
    String throwException = null;
    try {
      for (int index = 0; index < rules.size(); ++index) {
        Rule rule = rules.get(index);
        String result =
            evaluator.evaluate(reMapVariableKeysInPredicate(attributesGroup, rule.getPredicate()));
        EvaluationResult res = new EvaluationResult(result, EvaluationConstants.SINGLE_QUOTE);
        if (res.isBooleanFalse()) {
          continue;
        }

        queueId = rule.getQueueId();
        LOGGER.info("Matched to queue with ID={}", rule.getQueueId());
        break;
      }
    } catch (EvaluationException ex) {
      LOGGER.info("Evaluator::evaluate() failed with error: '{}", ex.getLocalizedMessage());
      throwException = ex.getLocalizedMessage();
    }
    if (throwException != null && !throwException.isEmpty()) {
      throw new EvaluatorException(throwException);
    }

    return queueId;
  }

  private Boolean evaluatePredicateByAttributes(AttributeGroupDto attributesGroup, String pridicate)
      throws CommsRouterException {
    if (pridicate == null || pridicate.isEmpty()) {
      return false;
    }
    Evaluator evaluator = new Evaluator();
    evaluator.putFunction(new HasFunction());
    evaluator.putFunction(new InFunction());
    evaluator.putFunction(new ContainsFunction());

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
            evaluator.putVariable(key, String.format("'%s'", value.getValue().toString()));
          }

          @Override
          public void handleLongValue(LongAttributeValueDto value) throws IOException {
            evaluator.putVariable(key, String.format("%d", (int) value.getValue()));
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
          public void handleArrayOfLongsValue(ArrayOfLongsAttributeValueDto value)
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

    String throwException = null;
    try {
      String result = evaluator.evaluate(reMapVariableKeysInPredicate(attributesGroup, predicate));
      EvaluationResult res = new EvaluationResult(result, EvaluationConstants.SINGLE_QUOTE);
      if (res.isBooleanFalse()) {
        return false;
      }

      LOGGER.info("Attribute={} matched to queue predicate={}", attributesGroup, predicate);
    } catch (EvaluationException ex) {
      LOGGER.info("Evaluator::evaluate() failed with error: '{}", ex.getLocalizedMessage());
      throwException = ex.getLocalizedMessage();
      // return false;
    }
    if (throwException != null && !throwException.isEmpty()) {
      throw new EvaluatorException(throwException);
    }

    return true;
  }

  private String reMapVariableKeysInPredicate(AttributeGroupDto attributesGroup, String predicate) {
    if (attributesGroup == null) {
      return predicate;
    }
    String result = predicate;
    Set<String> keys = attributesGroup.keySet();
    for (String key : keys) {
      String newKey = String.format(EVAL_VARIABLES_FORMAT, key);
      result = result.replaceAll(key, newKey);
    }
    return result;
  }

}
