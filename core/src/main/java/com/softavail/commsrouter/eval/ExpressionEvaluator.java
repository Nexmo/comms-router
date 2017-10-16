/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import net.sourceforge.jeval.EvaluationException;
import net.sourceforge.jeval.EvaluationHelper;
import net.sourceforge.jeval.Evaluator;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


/**
 *
 * @author ergyunsyuleyman
 */
public class ExpressionEvaluator extends Evaluator {

  private boolean isValidation = false;
  private static final Logger LOGGER = LogManager.getLogger(ExpressionEvaluator.class);

  @Override
  public String replaceVariables(final String expression) throws EvaluationException {
    String replacedVariable = EvaluatorHelpers.resolveBooleanVariable(expression);
    if (replacedVariable != null) {
      return replacedVariable;
    }

    if (isValidation) {
      return EvaluationHelper.replaceAll(expression, EvaluatorHelpers.VALIDATION_VARIABLE,
          EvaluatorHelpers.VALIDATION_VARIABLE_KEY_VALUE);
    }

    return super.replaceVariables(expression);
  }

  public void init() {
    init(false);
  }

  public void init(boolean isValidation) {
    this.isValidation = isValidation;
    putFunction(new HasFunction());
    putFunction(new InFunction());
    putFunction(new ContainsFunction());
    setVariableResolver(new CommsRouterVariableResolver(isValidation));
  }

  public boolean isValidExpression(String expression) {
    try {
      evaluate(expression);
      return true;
    } catch (EvaluationException ex) {
      LOGGER.info("Expression validation failed with: {}", ex.getLocalizedMessage());
    }
    return false;
  }
}
