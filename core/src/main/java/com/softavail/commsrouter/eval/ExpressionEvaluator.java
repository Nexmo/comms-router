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

import com.softavail.commsrouter.api.exception.EvaluatorException;
import net.sourceforge.jeval.EvaluationConstants;
import net.sourceforge.jeval.EvaluationException;
import net.sourceforge.jeval.Evaluator;



/**
 *
 * @author Ergyun Syuleyman
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

    if (isValidation()) {
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
    setPredicate(predicate);
    putFunction(new HasFunction(this));
    putFunction(new InFunction(this));
    putFunction(new ContainsFunction());
    setVariableResolver(new CommsRouterVariableResolver(this));
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


  public boolean isValidation() {
    return isValidation;
  }

  protected void setIsValidation(boolean isValidation) {
    this.isValidation = isValidation;
  }

  public void validateImpl(final String expression) throws EvaluatorException {
    try {
      this.setIsValidation(true);
      setPredicate(expression);
      super.evaluate(predicate);
    } catch (EvaluationException ex) {
      throw new EvaluatorException("Predicate \"" + expression + "\" failed with error: "
          + EvaluatorHelpers.getDetailedMessage(ex), ex);
    }
  }

  public String evaluateImpl() throws EvaluationException {
    if (predicateOrigin == null || predicateOrigin.isEmpty()) {
      return EvaluationConstants.BOOLEAN_STRING_FALSE;
    }
    
    this.setIsValidation(false);
    return super.evaluate(predicate);
  }

}
