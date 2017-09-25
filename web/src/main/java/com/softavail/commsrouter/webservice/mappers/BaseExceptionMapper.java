package com.softavail.commsrouter.webservice.mappers;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.ExceptionMapper;

/**
 * Created by @author mapuo on 03.09.17.
 */
public class BaseExceptionMapper<E extends Throwable> implements ExceptionMapper<E> {

  private static final Logger LOGGER = LogManager.getLogger(BaseExceptionMapper.class);

  @Context
  private HttpHeaders headers;

  private final Status status;

  public BaseExceptionMapper(Status status) {
    this.status = status;
  }

  @Override
  public Response toResponse(E exception) {

    LOGGER.error(exception.getMessage(), exception);

    return Response.status(status)
        .entity(getExceptionPresentation(exception))
        .type(headers.getMediaType())
        .build();
  }

  protected ExceptionPresentation getExceptionPresentation(E exception) {
    return new ExceptionPresentation(exception);
  }

}
