/*
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.softavail.commsrouter.eval;

import com.softavail.commsrouter.api.exception.EvaluatorException;

/**
 *
 * @author Ergyun Syuleyman
 */
public class CommsRouterEvaluatorFactory {

  private static enum ExpressionType {
    FALSE, TRUE, JEVAL, RSQL
  }

  private final RsqlEvaluatorFactory rsqlFactory = new RsqlEvaluatorFactory(this);

  private ExpressionType determineType(String expression) {

    if (expression == null) {
      return ExpressionType.FALSE;
    }
    expression = expression.trim().toLowerCase();
    if (expression.isEmpty() || expression.equals("false")) {
      return ExpressionType.FALSE;
    } else if (expression.equals("true")) {
      return ExpressionType.TRUE;
    } else if (expression.contains("#{") || expression.contains("contains(")
        || expression.contains("has(") || expression.contains("in(") || expression.equals("1==1")) {
      return ExpressionType.JEVAL;
    } else {
      return ExpressionType.RSQL;
    }
  }

  public CommsRouterEvaluator provide(String predicate) throws EvaluatorException {
    switch (determineType(predicate)) {
      case JEVAL:
        return new JEvalEvaluator(this, predicate);
      case RSQL:
        return rsqlFactory.create(predicate);
      case FALSE:
        return new FalseEvaluator(this);
      case TRUE:
        return new TrueEvaluator(this);
      default:
        throw new RuntimeException("Unexpected expression type: " + determineType(predicate));
    }
  }

  CommsRouterEvaluator changeExpression(JEvalEvaluator evaluator, String expression)
      throws EvaluatorException {

    ExpressionType type = determineType(expression);
    if (type == ExpressionType.JEVAL) {
      evaluator.replaceExpression(expression);
      return evaluator;
    }
    return provide(expression);
  }

  CommsRouterEvaluator changeExpression(EvaluatorBase evaluator, String expression)
      throws EvaluatorException {

    return provide(expression);
  }

}
