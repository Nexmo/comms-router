/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import net.sourceforge.jeval.VariableResolver;
import net.sourceforge.jeval.function.FunctionException;

/**
 *
 * @author ergyunsyuleyman
 */
public class CommsRouterVariableResolver implements VariableResolver {

  @Override
  public String resolveVariable(String variableName) throws FunctionException {
    return EvaluatorHelpers.resolveBooleanVariable(variableName);
  }

}
