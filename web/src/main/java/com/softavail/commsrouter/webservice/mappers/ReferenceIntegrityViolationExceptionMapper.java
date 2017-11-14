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

package com.softavail.commsrouter.webservice.mappers;

import com.softavail.commsrouter.api.exception.ExceptionPresentation;
import com.softavail.commsrouter.api.exception.ReferenceIntegrityViolationException;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

/**
 * Created by @author mapuo on 26.10.17.
 */
@Provider
public class ReferenceIntegrityViolationExceptionMapper
    extends BaseExceptionMapper<ReferenceIntegrityViolationException> {

  private static final Pattern REFERRED = Pattern.compile(
      "(delete\\son\\stable\\s|REFERENCES\\s)(?<q>[`\"])(?!fk_)(?<referred>\\w+)\\k<q>(?![.)])");

  private static final Pattern REFERRER = Pattern.compile(
      "(?<q>[`\"])(?!fk_)(?<referrer>\\w+)\\k<q>(?:\\s*Detail|.{2}CONSTRAINT)");


  public ReferenceIntegrityViolationExceptionMapper() {
    super(Status.INTERNAL_SERVER_ERROR);
  }

  @Override
  protected ExceptionPresentation getExceptionPresentation(
      ReferenceIntegrityViolationException exception) {

    String message = exception.getMessage();
    Throwable cveCause = exception.getCause();
    if (cveCause.getCause() != null) {
      message = cveCause.getCause().getMessage();
      Matcher referred = REFERRED.matcher(message);
      Matcher referrer = REFERRER.matcher(message);
      if (referred.find() && referrer.find()) {
        return new ExceptionPresentation(exception.getClass().getSimpleName(),
            String.format(
                "Cannot delete or update '%s' as there is record in '%s' that refer to it.",
                referred.group("referred"), referrer.group("referrer"))
        );
      }
    }

    return new ExceptionPresentation(exception.getClass().getSimpleName(), message);
  }

}
