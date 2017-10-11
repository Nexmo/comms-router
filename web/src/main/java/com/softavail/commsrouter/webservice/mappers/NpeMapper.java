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
