package com.softavail.commsrouter.nexmoapp.api.resources;

import com.softavail.commsrouter.api.interfaces.PaginatedService;
import com.softavail.commsrouter.nexmoapp.dto.model.SessionDto;
import com.softavail.commsrouter.nexmoapp.interfaces.SessionService;
import io.swagger.annotations.Api;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.container.ResourceContext;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 10.10.17.
 */
@Api
@Path("session")
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class SessionResource extends ApiObjectResource<SessionDto> {

  @Context
  private ResourceContext resourceContext;

  @Inject
  private SessionService sessionService;

  @Override
  protected PaginatedService<SessionDto> getService() {
    return sessionService;
  }

  @Override
  protected UriBuilder getEntryPoint() {
    return UriBuilder.fromResource(this.getClass());
  }

}
