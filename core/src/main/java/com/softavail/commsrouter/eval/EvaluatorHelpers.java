/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.eval;

import net.sourceforge.jeval.EvaluationConstants;
import net.sourceforge.jeval.function.FunctionException;
import net.sourceforge.jeval.function.FunctionHelper;

/**
 *
 * @author ergyunsyuleyman
 */
public class EvaluatorHelpers {

  public static String VALIDATION_VARIABLE_KEY_NAME = "CREValidationVar";
  public static String VALIDATION_VARIABLE_KEY_VALUE = EvaluationConstants.BOOLEAN_STRING_TRUE;
  public static String VALIDATION_VARIABLE = EvaluationConstants.OPEN_VARIABLE
      + VALIDATION_VARIABLE_KEY_NAME + EvaluationConstants.CLOSED_VARIABLE;

  public static String trimAndRemoveQuoteCharsIfNeed(final String input, final char quoteCharacter)
      throws FunctionException {
    String trimedValue = input.trim();
    if (trimedValue.charAt(0) == quoteCharacter
        && trimedValue.charAt(trimedValue.length() - 1) == quoteCharacter) {
      trimedValue = FunctionHelper.trimAndRemoveQuoteChars(trimedValue, quoteCharacter);
    }

    return trimedValue;
  }

  public static boolean isDouble(final String input) {
    try {
      Double.parseDouble(input);
    } catch (NumberFormatException ex) {
      return false;
    }
    return true;
  }

  public static String resolveBooleanVariable(final String variableName) {
    if (variableName.equals("true") || variableName.equals("TRUE")) {
      return EvaluationConstants.BOOLEAN_STRING_TRUE;
    }
    if (variableName.equals("false") || variableName.equals("FALSE")) {
      return EvaluationConstants.BOOLEAN_STRING_FALSE;
    }

    return null;
  }

}
