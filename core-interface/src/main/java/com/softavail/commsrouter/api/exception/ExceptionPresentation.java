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

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import java.util.Set;
import java.util.stream.Collectors;
import javax.validation.ConstraintViolation;

/**
 * Created by @author mapuo on 03.09.17.
 */
@JsonInclude(Include.NON_NULL)
public class ExceptionPresentation {

  @JsonInclude(Include.NON_NULL)
  class ErrorPresentation {

    public final String code;
    public final String description;

    public ErrorPresentation(String code, String description) {
      this.code = code;
      this.description = description;
    }

  }

  class HelpInformation {

    public final String url;

    HelpInformation(String url) {
      this.url = url;
    }

  }

  class ConstraintPresentation {

    public final String violation;

    ConstraintPresentation(String violation) {
      this.violation = violation;
    }

    ConstraintPresentation(ConstraintViolation violation) {
      this.violation = violation.getMessage();
    }

  }

  private final ErrorPresentation error;
  private final HelpInformation information;
  private final Set<ConstraintPresentation> violations;

  public ExceptionPresentation(Throwable exception) {
    this(exception.getClass().getSimpleName(), exception.getMessage());
  }

  public ExceptionPresentation(Throwable exception, String url) {
    this(exception.getClass().getSimpleName(), exception.getMessage(), url);
  }

  public ExceptionPresentation(String className, String message) {
    this(className, message, null, null);
  }

  public ExceptionPresentation(String className, String message, String url) {
    this.error = new ErrorPresentation(className, message);
    this.information = (url != null) ? new HelpInformation(url) : null;
    this.violations = null;
  }

  public ExceptionPresentation(String className, Set<ConstraintViolation<?>> violations) {
    this.error = new ErrorPresentation(className, null);
    this.information = null;
    this.violations = violations.stream()
        .map(ConstraintPresentation::new)
        .collect(Collectors.toSet());
  }

  public ExceptionPresentation(
      String className, String message, Set<ConstraintPresentation> violations) {
    this(className, message, null, violations);
  }

  public ExceptionPresentation(
      String className, String message, String url, Set<ConstraintPresentation> violations) {
    this.error = new ErrorPresentation(className, message);
    this.information = (url != null) ? new HelpInformation(url) : null;
    this.violations = violations;
  }

  public ErrorPresentation getError() {
    return error;
  }

  public HelpInformation getInformation() {
    return information;
  }

  public Set<ConstraintPresentation> getViolations() {
    return violations;
  }

}
