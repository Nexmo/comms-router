/* 
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.commsrouter.eval;

import net.sourceforge.jeval.VariableResolver;
import net.sourceforge.jeval.function.FunctionException;

/**
 *
 * @author Ergyun Syuleyman
 */
public class CommsRouterVariableResolver implements VariableResolver {

  private final ExpressionEvaluator evaluator;

  public CommsRouterVariableResolver(ExpressionEvaluator evaluator) {
    this.evaluator = evaluator;
  }

  @Override
  public String resolveVariable(String variableName)
      throws FunctionException, IllegalArgumentException {
    String resolvedName = EvaluatorHelpers.resolveBooleanVariable(variableName);
    if (resolvedName != null) {
      return resolvedName;
    }

    if (evaluator.isValidation()) {
      evaluator.isValidName(variableName);
      variableName = EvaluatorHelpers.VALIDATION_VARIABLE_VALUE;
      return variableName;
    }

    return null;
  }

}
