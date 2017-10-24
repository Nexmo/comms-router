package com.softavail.commsrouter.webservice.mappers;

import javax.persistence.RollbackException;
import javax.validation.ConstraintViolationException;
import javax.ws.rs.core.Response;
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
  public Response toResponse(RollbackException exception) {
    // Hibernate does not follow JPA 2 specs and wraps ConstraintViolation in RollbackException
    if (exception.getCause() instanceof ConstraintViolationException) {
      ConstraintViolationException cause = (ConstraintViolationException) exception.getCause();
      ConstraintViolationMapper mapper = new ConstraintViolationMapper();
      mapper.headers = headers;
      return mapper.toResponse(cause);
    }
    return super.toResponse(exception);
  }

}
