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
  private String predicate;
  private String predicateOrigin;

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

      EvaluatorHelpers.validationCheckSpecialCharsInVariable(expression);
    }

    return super.replaceVariables(expression);
  }

  public void init(String predicate) {
    init(predicate, false);
  }

  public void init(String predicate, boolean isValidation) {
    this.isValidation = isValidation;
    setPredicate(predicate);
    putFunction(new HasFunction(isValidation));
    putFunction(new InFunction(isValidation));
    putFunction(new ContainsFunction());
    setVariableResolver(new CommsRouterVariableResolver(isValidation, this));
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

  public void isValid() throws EvaluatorException {
    try {
      evaluateImpl();
    } catch (EvaluationException ex) {
      throw new EvaluatorException("Predicate \"" + predicateOrigin + "\" failed with error: "
          + EvaluatorHelpers.getDetailedMessage(ex), ex);
    }
  }

  public String evaluateImpl() throws EvaluationException {
    if (predicateOrigin == null || predicateOrigin.isEmpty()) {
      return EvaluationConstants.BOOLEAN_STRING_FALSE;
    }


      return super.evaluate(predicate);
  }
  
}
