/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import net.sourceforge.jeval.EvaluationException;
import net.sourceforge.jeval.Evaluator;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


/**
 *
 * @author ergyunsyuleyman
 */
public class ExpressionEvaluator extends Evaluator {

  private static final Logger LOGGER = LogManager.getLogger(ExpressionEvaluator.class);

  @Override
  public String replaceVariables(final String expression) throws EvaluationException {
    String replacedVariable = EvaluatorHelpers.resolveBooleanVariable(expression);
    if (replacedVariable != null) {
      return replacedVariable;
    }

    return super.replaceVariables(expression);
  }

  public void init() {
    putFunction(new HasFunction());
    putFunction(new InFunction());
    putFunction(new ContainsFunction());
    setVariableResolver(new CommsRouterVariableResolver());
  }

  public boolean isValidExpression(String expression) {
    try {
      String test = " HASS(colors,'red') || IN('support', skills)";
      parse(test);
      return true;
    } catch (EvaluationException ex) {
      LOGGER.info("Expression validation failed with: {}", ex.getLocalizedMessage());
    }
    return false;
  }
}
