/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import com.softavail.commsrouter.api.exception.ExpressionException;
import net.sourceforge.jeval.EvaluationException;
import net.sourceforge.jeval.Evaluator;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Map;
import java.util.Set;

/**
 *
 * @author ergyunsyuleyman
 */
public class EvaluatorExpressions {

  private static final Logger LOGGER = LogManager.getLogger(EvaluatorExpressions.class);

  public String replaceExpressionVariables(String expression, Set<String> variables)
      throws ExpressionException {
    Evaluator evaluator = new Evaluator();
    evaluator.putFunction(new HasFunction());
    evaluator.putFunction(new InFunction());
    evaluator.putFunction(new ContainsFunction());

    return resolveExpressionVariables(evaluator, expression, variables);
  }

  private String resolveExpressionVariables(Evaluator evaluator, String expression,
      Set<String> variables) throws ExpressionException {
    String result = expression;
    Map functions = evaluator.getFunctions();
    for (String variable : variables) {

      try {
        evaluator.isValidName(variable);
      } catch (IllegalArgumentException ex) {
        throw new ExpressionException(
            "Invalid variable name of \"" + variable + "\"." + ex.getLocalizedMessage());
      }
    }

    return result;
  }


  public boolean isValidExpression(String expression, Set<String> variables) {
    Evaluator evaluator = new Evaluator();
    evaluator.putFunction(new HasFunction());
    evaluator.putFunction(new InFunction());
    evaluator.putFunction(new ContainsFunction());
    try {
      String test = " HASS(colors,'red') || IN('support', skills)";
      evaluator.parse(test);
      return true;
    } catch (EvaluationException ex) {
      LOGGER.info("Expression validation failed with: {}", ex.getLocalizedMessage());
    }
    return false;
  }
}
