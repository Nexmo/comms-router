/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import com.softavail.commsrouter.api.exception.EvaluatorException;
import net.sourceforge.jeval.EvaluationConstants;
import net.sourceforge.jeval.EvaluationException;
import net.sourceforge.jeval.Evaluator;



/**
 *
 * @author ergyunsyuleyman
 */
public class ExpressionEvaluator extends Evaluator {

  private boolean isValidation = false;

  @Override
  public String replaceVariables(final String expression) throws EvaluationException {
    String replacedVariable = EvaluatorHelpers.resolveBooleanVariable(expression);
    if (replacedVariable != null) {
      return replacedVariable;
    }

    if (isValidation) {
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

      replacedVariable = EvaluatorHelpers.validationCheckSpecialCharsInVariable(expression);
      if (replacedVariable != null) {
        return replacedVariable;
      }
    }

    return super.replaceVariables(expression);
  }

  public void init() {
    init(false);
  }

  public void init(boolean isValidation) {
    this.isValidation = isValidation;
    putFunction(new HasFunction(isValidation));
    putFunction(new InFunction(isValidation));
    putFunction(new ContainsFunction());
    setVariableResolver(new CommsRouterVariableResolver(isValidation, this));
  }

  public void isValidExpression(String expression) throws EvaluatorException {
    try {
      String formatedExpression = EvaluatorHelpers.supportArraysInExpression(expression);
      evaluate(formatedExpression);
    } catch (EvaluationException ex) {
      throw new EvaluatorException(EvaluatorHelpers.getDetailedMessage(ex), ex);
    }
  }
}
