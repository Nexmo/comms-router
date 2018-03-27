/*
 * Copyright 2018 SoftAvail Inc.
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

import com.softavail.commsrouter.api.exception.ExpressionException;
import com.softavail.commsrouter.api.exception.ExpressionException;
import com.softavail.commsrouter.domain.AttributeGroup;
import cz.jirutka.rsql.parser.RSQLParser;
import cz.jirutka.rsql.parser.RSQLParserException;
import cz.jirutka.rsql.parser.ast.Node;

/**
 *
 * @author Vladislav Todorov
 */
public class RsqlEvaluatorFactory {

  private final CommsRouterEvaluatorFactory factory;

  public RsqlEvaluatorFactory(CommsRouterEvaluatorFactory factory) {
    this.factory = factory;
  }

  public Node parse(String expression) {
    return new RSQLParser().parse(expression);
  }

  public void validate(String expression) throws ExpressionException {
    try {
      parse(expression);
    } catch (RSQLParserException ex) {
      throw new ExpressionException("Invalid expression: " + ex.getMessage());
    }
  }

  public RsqlEvaluator create(String expression, String routerRef) throws ExpressionException {
    try {
      return new RsqlEvaluator(factory, parse(expression), routerRef);
    } catch (RSQLParserException ex) {
      throw new ExpressionException("Invalid expression: " + ex.getMessage());
    }
  }

  public boolean evaluate(String expression, AttributeGroup attributeGroup, String routerRef)
      throws ExpressionException {
    return create(expression, routerRef).evaluate(attributeGroup);
  }

}
