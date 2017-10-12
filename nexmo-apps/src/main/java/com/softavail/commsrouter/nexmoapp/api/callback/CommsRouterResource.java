package com.softavail.commsrouter.nexmoapp.api.callback;

import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.nexmoapp.interfaces.PluginService;
import com.softavail.commsrouter.nexmoapp.interfaces.SessionReferenceService;
import com.softavail.commsrouter.nexmoapp.interfaces.SessionService;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

/**
 * Created by @author mapuo on 09.10.17.
 */
@Api
@Path("{applicationId}/comms")
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class CommsRouterResource {

  @Inject
  private SessionService sessionService;

  @Inject
  private SessionReferenceService referenceService;

  @Inject
  private PluginService pluginService;

  @POST
  @ApiOperation("Processes incoming Task with assigned Agent from the Comms Router")
  public void taskAssigned(
      @PathParam("applicationId") String applicationId,
      TaskAssignmentDto assignmentDto)
      throws CommsRouterException {

    // TODO
  }

  @POST
  @Path("{sessionId}")
  public void taskAssigned(
      @PathParam("applicationId") String applicationId,
      @PathParam("sessionId") String sessionId,
      TaskAssignmentDto assignmentDto)
      throws CommsRouterException {

    // TODO
    sessionService.get(sessionId); // TODO

  }

}
