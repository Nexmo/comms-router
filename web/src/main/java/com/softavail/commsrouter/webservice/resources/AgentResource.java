/*
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.commsrouter.webservice.resources;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExceptionPresentation;
import com.softavail.commsrouter.api.interfaces.AgentService;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.webservice.helpers.GenericRouterObjectResource;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ResponseHeader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.EntityTag;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

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
      notes = "Add new Agent and associate it with a Router")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Successful operation", response = ApiObjectRef.class,
          responseHeaders = {
              @ResponseHeader(name = HttpHeaders.ETAG, description = "ETag of the resource",
                  response = String.class)})})
  public Response create(CreateAgentArg agentArg) throws CommsRouterException {

    LOGGER.debug("Creating agent {}", agentArg);

    ApiObjectRef agent = agentService.create(agentArg, routerRef);

    return createResponse(agent);
  }

  @PUT
  @Path("{resourceId}")
  @ApiOperation(
      value = "Replace an existing Agent",
      notes = "If the agent with the specified id does not exist, it creates it")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Successful operation", response = ApiObjectRef.class,
          responseHeaders = {
              @ResponseHeader(name = HttpHeaders.ETAG, description = "ETag of the resource",
                  response = String.class)}),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Agent not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public Response create(
      @ApiParam(value = "The id of the agent to be replaced", required = true)
      @PathParam("resourceId")
          String resourceId,
      @ApiParam(value = "CreateAgentArg object specifying all the parameters")
          CreateAgentArg agentArg)
      throws CommsRouterException {

    LOGGER.debug("Replacing agent: {}, with id: {}", agentArg, resourceId);

    RouterObjectRef objectRef = getRouterObjectRef(resourceId);

    ApiObjectRef agent = agentService.replace(agentArg, objectRef);

    return createResponse(agent);
  }

  @POST
  @Path("{resourceId}")
  @ApiOperation(
      value = "Update an existing Agent",
      notes = "Update some properties of an existing Agent")
  @ApiResponses({
      @ApiResponse(code = 204, message = "Successful operation", responseHeaders = {
          @ResponseHeader(name = HttpHeaders.ETAG, description = "ETag of the resource",
              response = String.class)}),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Agent not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 412, message = "Precondition Failed",
          response = ExceptionPresentation.class)})
  public Response update(
      @ApiParam(value = "ETag header from creating or retrieving resource", required = true)
      @HeaderParam(HttpHeaders.IF_MATCH)
          String ifMatch,
      @ApiParam(value = "ID of the agent to be updated")
      @PathParam("resourceId")
          String resourceId,
      @ApiParam(
          value = "UpdateAgentArg object representing parameters of the Agent to be updated",
          required = true)
          UpdateAgentArg agentArg)
      throws CommsRouterException {

    LOGGER.debug("Updating agent {}", agentArg);

    RouterObjectRef objectId = getRouterObjectRef(resourceId);
    objectId.setHash(ifMatch);

    agentService.update(agentArg, objectId);
    AgentDto updatedAgent = agentService.get(objectId);

    return Response.status(Status.NO_CONTENT)
        .tag(new EntityTag(updatedAgent.getHash()))
        .build();
  }

}
