package com.softavail.commsrouter.webservice.mappers;

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

}
