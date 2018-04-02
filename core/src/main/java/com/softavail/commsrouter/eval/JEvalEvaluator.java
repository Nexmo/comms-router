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

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExpressionException;
import com.softavail.commsrouter.domain.AttributeGroup;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author Ergyun Syuleyman
 */
public class JEvalEvaluator implements CommsRouterEvaluator {

  private final CommsRouterEvaluatorFactory factory;
  private final ExpressionEvaluator evaluator;

  private static final Logger LOGGER = LogManager.getLogger(JEvalEvaluator.class);

  public JEvalEvaluator(CommsRouterEvaluatorFactory factory, String predicate) {
    this.factory = factory;
    evaluator = new ExpressionEvaluator();
    evaluator.init(predicate);
  }

  @Override
  public CommsRouterEvaluator changeExpression(String expression, String routerRef)
      throws ExpressionException {

    return factory.changeExpression(this, expression, routerRef);
  }

  void replaceExpression(String expression) {
    evaluator.setPredicate(expression);
  }

  /**
   *
   * @throws ExpressionException .
   */
  @Override
  public void validate() throws ExpressionException {

    long millis = System.currentTimeMillis();
    evaluator.validateImpl();
    LOGGER.trace("Predicate expression validation time is: {}",
        (System.currentTimeMillis() - millis));
  }

  /**
   *
   * @param attributesGroup evaluator variable attributes using on predicate expression evaluate
   * @return true - if match success
   * @throws CommsRouterException .
   */
  @Override
  public boolean evaluate(AttributeGroup attributesGroup) throws CommsRouterException {
    if (evaluator == null) {
      throw new ExpressionException("Predicate evaluator is not initialized with expression value. "
          + "Please call 'init(String predicate)' first.");
    }
    if (evaluator.evaluate(attributesGroup)) {
      LOGGER.info("Attributes={} matched to predicate={}", attributesGroup,
          evaluator.getPredicate());
      return true;
    }
    
    return false;
  }

}
