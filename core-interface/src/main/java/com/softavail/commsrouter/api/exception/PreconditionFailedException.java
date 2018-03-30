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

package com.softavail.commsrouter.api.exception;

/**
 * Created by @author mapuo on 29/03/18.
 */
public class PreconditionFailedException extends CommsRouterException {

  private String expected;
  private String actual;

  public PreconditionFailedException() {
    super("Resource version mismatch.");
  }

  public PreconditionFailedException(String expected, String actual) {
    super("Resource version mismatch. Version '" + actual + "' is not valid.");
    this.expected = expected;
    this.actual = actual;
  }

  public PreconditionFailedException(Throwable cause) {
    super(cause);
  }

  public PreconditionFailedException(String message, Throwable cause) {
    super(message, cause);
  }

  public String getExpected() {
    return expected;
  }

  public String getActual() {
    return actual;
  }

}
