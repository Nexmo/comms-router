package com.softavail.commsrouter.webservice.mappers;

import javax.validation.ValidationException;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

/**
 * Created by @author mapuo on 06.10.17.
 */
@Provider
public class ValidationExceptionMapper extends BaseExceptionMapper<ValidationException> {

  public ValidationExceptionMapper() {
    super(Status.BAD_REQUEST);
  }

  @Override
  protected ExceptionPresentation getExceptionPresentation(ValidationException exception) {
    return new ExceptionPresentation(
        exception.getClass().getSimpleName(), exception.getMessage());
  }

}
