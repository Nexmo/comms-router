/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import net.sourceforge.jeval.EvaluationConstants;
import net.sourceforge.jeval.VariableResolver;
import net.sourceforge.jeval.function.FunctionException;

/**
 *
 * @author ergyunsyuleyman
 */
public class CommsRouterVariableResolver implements VariableResolver {

  private boolean isValidation = false;

  public CommsRouterVariableResolver(boolean isValidation) {
    this.isValidation = isValidation;
  }

  @Override
  public String resolveVariable(String variableName) throws FunctionException {
    String resolvedName = EvaluatorHelpers.resolveBooleanVariable(variableName);
    if (resolvedName != null) {
      return resolvedName;
    }

    if (isValidation) {
      if (variableName.startsWith(EvaluationConstants.OPEN_VARIABLE)
          && variableName.endsWith(EvaluationConstants.CLOSED_VARIABLE)) {
        return EvaluatorHelpers.VALIDATION_VARIABLE;
      }
    }

    return variableName;
  }

}
