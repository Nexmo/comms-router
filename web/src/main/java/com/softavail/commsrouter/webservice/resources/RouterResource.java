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

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExceptionPresentation;
import com.softavail.commsrouter.api.interfaces.PaginatedService;
import com.softavail.commsrouter.api.service.CoreRouterService;
import com.softavail.commsrouter.api.service.PaginationHelper;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ResponseHeader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.URI;
import java.net.URL;
import java.util.List;
import java.util.stream.Collectors;
import javax.inject.Inject;
import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Pattern;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.container.ResourceContext;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.EntityTag;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 31.08.17.
 */
@Path("routers")
@Api()
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class RouterResource {

  private static final Logger LOGGER = LogManager.getLogger(RouterResource.class);

  @Context
  private ResourceContext resourceContext;

  @Inject
  private CoreRouterService routerService;

  @GET
  @ApiOperation(value = "Get All Routers",
      notes = "Returns a list of Router. A single Router object represents "
          + "a container for Agents, Tasks, Queues and Plans.",
      response = RouterDto.class,
      responseContainer = "List",
      tags = "routers")
  @ApiResponses(
      @ApiResponse(code = 200, message = "Successful operation", responseHeaders = {
          @ResponseHeader(
              name = PaginatedService.NEXT_TOKEN_HEADER,
              description = "The token for the next page",
              response = String.class),
          @ResponseHeader(
              name = HttpHeaders.ETAG,
              description = "ETags of the resources in the list separated by semicolon",
              response = String.class)}))
  public Response list(
      @ApiParam(
          value = "The token from the previous request",
          defaultValue = "")
      @Valid
      @QueryParam(PaginatedService.TOKEN_PARAM) String token,
      @ApiParam(
          value = "Number of items per page (Maximum 50)",
          defaultValue = "10",
          allowableValues = "range[1, 50]")
      @Valid
      @Min(value = 1L, message = "{resource.list.min.items.per.page}")
      @Max(value = 50, message = "{resource.list.max.items.per.page}")
      @DefaultValue("10")
      @QueryParam(PaginatedService.ITEMS_PER_PAGE_PARAM) int perPage,
      @Valid
      @Pattern(regexp = PaginationHelper.SORT_REGEX, message = "{resource.list.sort.message}")
      @QueryParam(PaginatedService.SORT_PARAM) String sort,
      @QueryParam(PaginatedService.QUERY_PARAM) String query)
      throws CommsRouterException {

    PagingRequest pagingRequest = new PagingRequest(token, perPage, sort, query);

    PaginatedList<RouterDto> pagedList = routerService.list(pagingRequest);

    LOGGER.debug("Listing all routers: {}", pagedList);

    GenericEntity<List<RouterDto>> genericEntity =
        new GenericEntity<>(pagedList.getList(), List.class);

    String tags = pagedList.getList().stream()
        .map(ApiObjectRef::getHash)
        .collect(Collectors.joining(";"));

    return Response.ok()
        .header(PaginatedService.NEXT_TOKEN_HEADER, pagedList.getNextToken())
        .tag(new EntityTag(tags))
        .entity(genericEntity)
        .type(MediaType.APPLICATION_JSON_TYPE)
        .build();
  }

  @GET
  @Path("{id}")
  @ApiOperation(
      value = "Find router by ID",
      notes = "Searches all routers by the given ID",
      response = RouterDto.class,
      tags = "routers")
  @ApiResponses({
      @ApiResponse(code = 200, message = "Successful operation", responseHeaders = {
          @ResponseHeader(name = HttpHeaders.ETAG, description = "ETag of the resource",
              response = String.class)}),
      @ApiResponse(code = 304, message = "Not Modified", responseHeaders = {
          @ResponseHeader(name = HttpHeaders.ETAG, description = "ETag of the resource",
              response = String.class)}),
      @ApiResponse(code = 404, message = "Router with the provided id is not found",
          response = ExceptionPresentation.class)})
  public Response get(
      @Context Request request,
      @ApiParam(value = "ETag header provided when creating or retrieving resource")
      @HeaderParam(HttpHeaders.IF_NONE_MATCH) String ifNoneMatch,
      @ApiParam(value = "ID of the router to be searched")
      @PathParam("id") String id)
      throws CommsRouterException {

    LOGGER.debug("Looking for router with ref: {}", id);

    RouterDto routerDto = routerService.get(id);
    EntityTag entityTag = new EntityTag(routerDto.getHash());

    ResponseBuilder builder = request.evaluatePreconditions(entityTag);

    if (builder != null) {
      return builder.build();
    }

    return Response.ok()
        .tag(entityTag)
        .entity(routerDto)
        .build();
  }

  @POST
  @ApiOperation(
      value = "Create a Router",
      notes = "A Router is a container for your Tasks, Agents, Tasks, Plans and Rules.",
      response = ApiObjectRef.class,
      code = 201,
      tags = "routers")
  @ApiResponses(
      @ApiResponse(code = 201, message = "Created", responseHeaders = {
          @ResponseHeader(name = HttpHeaders.LOCATION, response = URL.class,
              description = "The path to the newly created resource"),
          @ResponseHeader(name = HttpHeaders.ETAG, response = String.class,
              description = "ETag of the resource")}))
  public Response create(
      @ApiParam(value = "Router object to be added to the list of routers", required = true)
          CreateRouterArg routerArg)
      throws CommsRouterException {

    LOGGER.debug("Creating router: {}", routerArg);

    ApiObjectRef router = routerService.create(routerArg);

    URI createLocation =
        UriBuilder.fromResource(this.getClass()).path("{id}").build(router.getRef());

    return Response.status(Status.CREATED)
        .header(HttpHeaders.LOCATION, createLocation.toString())
        .tag(new EntityTag(router.getHash()))
        .entity(new ApiObjectRef(router))
        .build();
  }

  @POST
  @Path("{id}")
  @ApiOperation(value = "Update an existing router properties", tags = "routers")
  @ApiResponses({
      @ApiResponse(code = 204, message = "Successful operation", responseHeaders = {
          @ResponseHeader(name = HttpHeaders.ETAG, response = String.class,
              description = "ETag of the resource")}),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Router not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 412, message = "Precondition Failed",
          response = ExceptionPresentation.class)})
  public Response update(
      @ApiParam(value = "ETag header from creating or retrieving resource", required = true)
      @HeaderParam(HttpHeaders.IF_MATCH)
          String ifMatch,
      @ApiParam(value = "The id of the router to be updated", required = true)
      @PathParam("id")
          String id,
      @ApiParam(
          value = "UpdateRouterArg object specifying parameters to be updated",
          required = true)
          UpdateRouterArg routerArg)
      throws CommsRouterException {

    LOGGER.debug("Updating router: {}", routerArg);

    ApiObjectRef objectRef = new ApiObjectRef(id);
    objectRef.setHash(ifMatch);

    routerService.update(routerArg, objectRef);
    RouterDto updatedRouter = routerService.get(id);

    return Response.status(Status.NO_CONTENT)
        .tag(new EntityTag(updatedRouter.getHash()))
        .build();
  }

  @PUT
  @Path("{ref}")
  @ApiOperation(
      value = "Replace an existing router",
      notes = "If the router with the specified id does not exist, it creates it",
      tags = "routers")
  @ApiResponses({
      @ApiResponse(code = 200, message = "Successful operation", responseHeaders = {
          @ResponseHeader(name = HttpHeaders.ETAG, response = String.class,
              description = "ETag of the resource")}),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Router not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public Response replace(
      @ApiParam(
          value = "The id of the router to be updated",
          required = true) @PathParam("ref") String ref,
      @ApiParam(value = "CreateRouterArg object specifying all the parameters",
          required = true) CreateRouterArg routerArg)
      throws CommsRouterException {

    LOGGER.debug("Replacing router: {}, with ref: {}", routerArg, ref);

    ApiObjectRef router = routerService.replace(routerArg, ref);

    URI createLocation =
        UriBuilder.fromResource(this.getClass()).path("{ref}").build(router.getRef());

    return Response.status(Status.CREATED)
        .header(HttpHeaders.LOCATION, createLocation.toString())
        .tag(new EntityTag(router.getHash()))
        .entity(router)
        .build();
  }

  @DELETE
  @Path("{ref}")
  @ApiOperation(value = "Deletes an existing router by ID", tags = "routers")
  @ApiResponses({
      @ApiResponse(code = 200, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Router not found",
          response = ExceptionPresentation.class)})
  public void delete(@ApiParam(value = "The id of the router to be deleted",
      required = true) @PathParam("ref") String ref) throws CommsRouterException {

    LOGGER.debug("Deleting router: {}", ref);

    routerService.delete(ref);
  }

  // Sub-resources

  @Path("{routerRef}/plans")
  @ApiOperation(
      value = "Plans sub-resource",
      response = PlanResource.class,
      tags = "plans")
  public PlanResource planResource(@PathParam("routerRef") String routerRef) {
    LOGGER.debug("Router {} plans", routerRef);

    PlanResource resource = resourceContext.getResource(PlanResource.class);
    resource.setRouterRef(routerRef);
    UriBuilder planResource =
        UriBuilder.fromResource(this.getClass()).path(this.getClass(), "planResource");
    resource.setEntryPoint(planResource);
    return resource;
  }

  @Path("{routerRef}/queues")
  @ApiOperation(
      value = "Queues sub-resource",
      response = QueueResource.class,
      tags = "queues")
  public QueueResource queueResource(@PathParam("routerRef") String routerRef) {
    LOGGER.debug("Router {} queues", routerRef);

    QueueResource resource = resourceContext.getResource(QueueResource.class);
    resource.setRouterRef(routerRef);
    UriBuilder queueResource =
        UriBuilder.fromResource(this.getClass()).path(this.getClass(), "queueResource");
    resource.setEntryPoint(queueResource);
    return resource;
  }

  @Path("{routerRef}/tasks")
  @ApiOperation(
      value = "Tasks sub-resource",
      response = TaskResource.class,
      tags = "tasks")
  public TaskResource taskResource(@PathParam("routerRef") String routerRef) {
    LOGGER.debug("Router {} tasks", routerRef);

    TaskResource resource = resourceContext.getResource(TaskResource.class);
    resource.setRouterRef(routerRef);
    UriBuilder taskResource =
        UriBuilder.fromResource(this.getClass()).path(this.getClass(), "taskResource");
    resource.setEntryPoint(taskResource);
    return resource;
  }

  @Path("{routerRef}/agents")
  @ApiOperation(
      value = "Agents sub-resource",
      response = AgentResource.class,
      tags = "agents")
  public AgentResource agentResource(@PathParam("routerRef") String routerRef) {
    LOGGER.debug("Router {} agents", routerRef);

    AgentResource resource = resourceContext.getResource(AgentResource.class);
    resource.setRouterRef(routerRef);
    UriBuilder agentResource =
        UriBuilder.fromResource(this.getClass()).path(this.getClass(), "agentResource");
    resource.setEntryPoint(agentResource);
    return resource;
  }

  @Path("{routerRef}/skills")
  @ApiOperation(
      value = "Skills sub-resource",
      response = AgentResource.class,
      tags = "skills")
  public SkillResource skillResource(@PathParam("routerRef") String routerRef) {
    LOGGER.debug("Router {} skills", routerRef);

    SkillResource resource = resourceContext.getResource(SkillResource.class);
    resource.setRouterRef(routerRef);
    UriBuilder agentResource =
        UriBuilder.fromResource(this.getClass()).path(this.getClass(), "skillResource");
    resource.setEntryPoint(agentResource);
    return resource;
  }

}
