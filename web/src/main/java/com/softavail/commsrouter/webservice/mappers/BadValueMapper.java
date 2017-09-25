package com.softavail.commsrouter.webservice.mappers;

import com.softavail.commsrouter.api.exception.BadValueException;

import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

/**
 * Created by @author mapuo on 03.09.17.
 */
@Provider
public class BadValueMapper extends BaseExceptionMapper<BadValueException> {

  public BadValueMapper() {
    super(Status.BAD_REQUEST);
  }

}
