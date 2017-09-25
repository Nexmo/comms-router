package com.softavail.commsrouter.webservice.resources;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.AgentService;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.webservice.helpers.GenericRouterObjectResource;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 * Created by @author mapuo on 31.08.17.
 */
@Api("/agents")
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class AgentResource extends GenericRouterObjectResource<AgentDto> {

  private static final Logger LOGGER = LogManager.getLogger(AgentResource.class);

  @Inject
  private AgentService agentService;

  @Override
  protected RouterObjectService<AgentDto> getService() {
    return agentService;
  }

  @POST
  @ApiOperation(
      value = "Add new Agent",
      notes = "Add new Agent and associate it with a Router",
      response = ApiObject.class)
  public Response create(CreateAgentArg agentArg)
      throws CommsRouterException {

    agentArg.setRouterId(routerId);
    LOGGER.debug("Creating agent {}", agentArg);

    AgentDto agent = agentService.create(agentArg);

    return createResponse(agent);
  }

  @PUT
  @Path("{resourceId}")
  @ApiOperation(
      value = "Update an existing Agent",
      notes = "Update some properties of an existing Agent")
  public void update(
      @ApiParam(value = "ID of the agent to be updated")
      @PathParam("resourceId")
          String resourceId,
      @ApiParam(
          value = "UpdateAgentArg object representing parameters of the Agent to be updated",
          required = true)
          UpdateAgentArg agentArg)
      throws CommsRouterException {

    agentArg.setRouterId(routerId);
    agentArg.setId(resourceId);
    LOGGER.debug("Updating agent {}", agentArg);

    agentService.update(agentArg);
  }

}
