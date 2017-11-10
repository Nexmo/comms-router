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

import net.sourceforge.jeval.EvaluationConstants;
import net.sourceforge.jeval.Evaluator;
import net.sourceforge.jeval.function.Function;
import net.sourceforge.jeval.function.FunctionConstants;
import net.sourceforge.jeval.function.FunctionException;
import net.sourceforge.jeval.function.FunctionHelper;
import net.sourceforge.jeval.function.FunctionResult;

import java.util.ArrayList;

/**
 * This class is a function that executes within Evaluator. The function returns true if the source
 * string contains the substring.
 */
public class ContainsFunction implements Function {
  /**
   * Returns the name of the function - "CONTAINS".
   * 
   * @return The name of this function class.
   */
  public String getName() {
    return "CONTAINS";
  }

  /**
   * Executes the function for the specified argument. This method is called internally by
   * Evaluator.
   * 
   * @param evaluator An instance of Evaluator.
   * @param arguments A string argument that will be converted into two string arguments. The first
   *        argument is the source string, the second argument is the substring. The string
   *        argument(s) HAS to be enclosed in quotes. White space that is not enclosed within quotes
   *        will be trimmed. Quote characters in the first and last positions of any string argument
   *        (after being trimmed) will be removed also. The quote characters used must be the same
   *        as the quote characters used by the current instance of Evaluator. If there are multiple
   *        arguments, they must be separated by a comma (",").
   * 
   * @return Returns "1.0" (true) if the string ends with the suffix, otherwise it returns "0.0"
   *         (false). The return value represents a Boolean value that is compatible with the
   *         Boolean operators used by Evaluator.
   * 
   * @exception FunctionException Thrown if the argument(s) are not valid for this function.
   */
  @Override
  public FunctionResult execute(final Evaluator evaluator, final String arguments)
      throws FunctionException {
    String result = null;
    String exceptionMessage = "Two string arguments are required.";

    ArrayList<?> values = FunctionHelper.getTwoStringsAndOneInteger(arguments,
        EvaluationConstants.FUNCTION_ARGUMENT_SEPARATOR);

    if (values.size() != 2) {
      throw new FunctionException(exceptionMessage);
    }

    {
      String argumentOne = EvaluatorHelpers.trimAndRemoveQuoteCharsIfNeed((String) values.get(0),
          evaluator.getQuoteCharacter());
      String argumentTwo = EvaluatorHelpers.trimAndRemoveQuoteCharsIfNeed((String) values.get(1),
          evaluator.getQuoteCharacter());
      Integer indexOf = argumentOne.indexOf(argumentTwo, 0);
      if (indexOf >= 0) {
        result = EvaluationConstants.BOOLEAN_STRING_TRUE;
      } else {
        result = EvaluationConstants.BOOLEAN_STRING_FALSE;
      }
    }

    return new FunctionResult(result, FunctionConstants.FUNCTION_RESULT_TYPE_NUMERIC);
  }
}
