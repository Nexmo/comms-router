package com.softavail.commsrouter.webservice.mappers;

import javax.validation.ConstraintViolationException;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

/**
 * Created by @author mapuo on 06.10.17.
 */
@Provider
public class ConstraintViolationMapper extends BaseExceptionMapper<ConstraintViolationException> {

  public ConstraintViolationMapper() {
    super(Status.BAD_REQUEST);
  }

  @Override
  protected ExceptionPresentation getExceptionPresentation(ConstraintViolationException exception) {
    return new ExceptionPresentation(
        exception.getClass().getSimpleName(), exception.getConstraintViolations());
  }

}
