/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import net.sourceforge.jeval.Evaluator;
import net.sourceforge.jeval.VariableResolver;
import net.sourceforge.jeval.function.FunctionException;

/**
 *
 * @author ergyunsyuleyman
 */
public class CommsRouterVariableResolver implements VariableResolver {

  private boolean isValidation = false;
  private Evaluator evaluator;

  public CommsRouterVariableResolver(boolean isValidation, Evaluator evaluator) {
    this.isValidation = isValidation;
    this.evaluator = evaluator;
  }

  @Override
  public String resolveVariable(String variableName)
      throws FunctionException, IllegalArgumentException {
    String resolvedName = EvaluatorHelpers.resolveBooleanVariable(variableName);
    if (resolvedName != null) {
      return resolvedName;
    }

    if (isValidation) {
      evaluator.isValidName(variableName);
      variableName = EvaluatorHelpers.VALIDATION_VARIABLE_VALUE;
      return variableName;
    }

    return null;
  }

}
