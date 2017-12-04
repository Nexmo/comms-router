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

import javax.persistence.RollbackException;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

/**
 * Created by @author mapuo on 24.10.17.
 */
@Provider
public class RollbackExceptionMapper extends BaseExceptionMapper<RollbackException> {

  public RollbackExceptionMapper() {
    super(Status.INTERNAL_SERVER_ERROR);
  }

  @Override
  protected ExceptionPresentation getExceptionPresentation(RollbackException exception) {
    if (exception.getCause() != null) {
      return new ExceptionPresentation(exception.getCause());
    }
    return super.getExceptionPresentation(exception);
  }

}
