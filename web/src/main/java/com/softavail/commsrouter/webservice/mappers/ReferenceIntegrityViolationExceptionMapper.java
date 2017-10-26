package com.softavail.commsrouter.webservice.mappers;

import com.softavail.commsrouter.api.exception.ReferenceIntegrityViolationException;

import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

/**
 * Created by @author mapuo on 26.10.17.
 */
@Provider
public class ReferenceIntegrityViolationExceptionMapper
    extends BaseExceptionMapper<ReferenceIntegrityViolationException> {

  public ReferenceIntegrityViolationExceptionMapper() {
    super(Status.INTERNAL_SERVER_ERROR);
  }

}
