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

import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

/**
 * Created by @author mapuo on 03.09.17.
 */
@Provider
public class NpeMapper extends BaseExceptionMapper<NullPointerException> {

  public NpeMapper() {
    super(Status.INTERNAL_SERVER_ERROR);
  }

  @Override
  protected ExceptionPresentation getExceptionPresentation(NullPointerException exception) {
    return new ExceptionPresentation(
        exception.getClass().getSimpleName(),
        "NULLS, NULLS EVERYWHERE",
        "https://www.google.bg/search?q=nulls+everywhere&tbm=isch");
  }

}
