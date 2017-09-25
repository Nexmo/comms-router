package com.softavail.commsrouter.webservice.mappers;

import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

/**
 * Created by @author mapuo on 03.09.17.
 */
@Provider
public class RuntimeExceptionMapper extends BaseExceptionMapper<RuntimeException> {

  public RuntimeExceptionMapper() {
    super(Status.INTERNAL_SERVER_ERROR);
  }

}
