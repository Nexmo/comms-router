/*
 * Copyright 2017 - 2018 SoftAvail Inc.
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

import com.softavail.commsrouter.api.dto.arg.CreateSkillArg;
import com.softavail.commsrouter.api.dto.arg.UpdateSkillArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.skill.SkillDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExceptionPresentation;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.api.interfaces.SkillService;
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
 *
 * @author ikrustev
 */
@Api("/skills")
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class SkillResource extends GenericRouterObjectResource<SkillDto> {

  private static final Logger LOGGER = LogManager.getLogger(SkillResource.class);

  @Inject
  private SkillService service;

  @Override
  protected RouterObjectService<SkillDto> getService() {
    return service;
  }

  @POST
  @ApiOperation(
      value = "Add new Skill",
      notes = "Add new Skill and associate it with a Router")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Successful operation", response = ApiObjectRef.class,
          responseHeaders = {
              @ResponseHeader(name = HttpHeaders.ETAG, description = "ETag of the resource",
                  response = String.class)})})
  public Response create(CreateSkillArg skillArg) throws CommsRouterException {

    LOGGER.debug("Creating skill {}", skillArg);

    ApiObjectRef skill = service.create(skillArg, routerRef);

    return createResponse(skill);
  }

  @PUT
  @Path("{resourceRef}")
  @ApiOperation(
      value = "Replace an existing Skill",
      notes = "If no skill with the specified ref exists, it creates it")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Successful operation", response = ApiObjectRef.class,
          responseHeaders = {
              @ResponseHeader(name = HttpHeaders.ETAG, description = "ETag of the resource",
                  response = String.class)}),
      @ApiResponse(code = 400, message = "Invalid ref supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public Response create(
      @ApiParam(value = "The ref of the Skill to be replaced", required = true)
      @PathParam("resourceRef")
          String resourceRef,
      @ApiParam(value = "CreateSkillArg object specifying all the parameters")
          CreateSkillArg skillArg)
      throws CommsRouterException {

    LOGGER.debug("Replacing skill: {}, with ref: {}", skillArg, resourceRef);

    RouterObjectRef objectRef =
        RouterObjectRef.builder().setRef(resourceRef).setRouterRef(routerRef).build();

    ApiObjectRef skill = service.replace(skillArg, objectRef);

    return createResponse(skill);
  }

  @POST
  @Path("{resourceRef}")
  @ApiOperation(
      value = "Update an existing Skill",
      notes = "Update some properties of an existing Skill")
  @ApiResponses({
      @ApiResponse(code = 204, message = "Successful operation", responseHeaders = {
              @ResponseHeader(name = HttpHeaders.ETAG, description = "ETag of the resource",
                  response = String.class)}),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Skill not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 412, message = "Precondition Failed",
          response = ExceptionPresentation.class)})
  public Response update(
      @ApiParam(value = "ETag header from creating or retrieving resource", required = true)
      @HeaderParam(HttpHeaders.IF_MATCH)
          String ifMatch,
      @ApiParam(value = "Ref of the Skill to be updated")
      @PathParam("resourceRef")
          String resourceId,
      @ApiParam(
          value = "UpdateSkillArg object representing parameters of the Skill to be updated",
          required = true)
          UpdateSkillArg skillArg)
      throws CommsRouterException {

    LOGGER.debug("Updating skill {}", skillArg);

    RouterObjectRef objectId = getRouterObjectRef(resourceId);
    objectId.setHash(ifMatch);

    service.update(skillArg, objectId);
    SkillDto updatedSkill = service.get(objectId);

    return Response.status(Status.NO_CONTENT)
        .tag(new EntityTag(updatedSkill.getHash()))
        .build();
  }

}
