/*
 * Copyright 2018 SoftAvail Inc.
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
import com.softavail.commsrouter.api.exception.ExpressionException;
import java.util.List;

/**
 *
 * @author vladislav
 */
public class ValidationUtils {

  public static void validateArguments(String operator, List arguments) throws ExpressionException {
    switch (operator) {
      case "==":
      case "!=":
      case "=gt=":
      case ">":
      case "=ge=":
      case ">=":
      case "=lt=":
      case "<":
      case "=le=":
      case "<=":
        assertSingleParameter(operator, arguments);
        break;
      case "=in=":
      case "=out=":
        break;
      default:
        throw new ExpressionException("Unsupported operator: " + operator);
    }
  }

  public static void validateAttributes(String operator, List attributes)
      throws ExpressionException {
    switch (operator) {
      case "=gt=":
      case ">":
      case "=ge=":
      case ">=":
      case "=lt=":
      case "<":
      case "=le=":
      case "<=":
        assertSingleParameter(operator, attributes);
        break;
      case "==":
      case "!=":
      case "=in=":
      case "=out=":
        break;
      default:
        throw new ExpressionException("Unsupported operator: " + operator);
    }
  }

  public static void assertSingleParameter(String operator, List element)
      throws EvaluatorException {
    if (element.size() != 1) {
      throw new EvaluatorException("Invalid arguments number for operator '" + operator
          + "'. Expected 1 but found " + element.size());
    }
  }

  public static Boolean parseBoolean(String bool) throws ExpressionException {
    if (bool != null) {
      String b = bool.toLowerCase();
      if (b.equals("true") || b.equals("false")) {
        return Boolean.parseBoolean(b);
      }
    }
    throw new ExpressionException("Invalid boolean argument:" + bool);
  }

  public static Double parseNumber(String number) throws ExpressionException {
    try {
      return Double.parseDouble(number);
    } catch (NumberFormatException ex) {
      throw new ExpressionException("Invalid number argument:" + number);
    }
  }
}
