package com.softavail.commsrouter.nexmoapp.api;

import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

/**
 * Created by @author mapuo on 09.10.17.
 */
@Path("{applicationId}/comms")
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class CommsRouterResource {

  @POST
  public void taskAssigned(
      @PathParam("applicationId") String applicationId,
      TaskAssignmentDto assignmentDto) {
    // TODO 
  }

}
