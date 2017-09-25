package com.softavail.commsrouter.webservice.mappers;

import com.softavail.commsrouter.api.exception.NotFoundException;

import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

/**
 * Created by @author mapuo on 31.08.17.
 */
@Provider
public class NotFoundMapper extends BaseExceptionMapper<NotFoundException> {

  public NotFoundMapper() {
    super(Status.NOT_FOUND);
  }

}
