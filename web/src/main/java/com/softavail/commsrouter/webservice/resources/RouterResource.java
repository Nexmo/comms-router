package com.softavail.commsrouter.webservice.resources;

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExceptionPresentation;
import com.softavail.commsrouter.api.service.CoreRouterService;
import com.softavail.commsrouter.app.TaskDispatcher;
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
import java.util.Collection;
import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.container.ResourceContext;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
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

  @Inject
  private TaskDispatcher taskDispatcher;

  @GET
  @ApiOperation(value = "Get All Routers",
      notes = "Returns a list of Router. A single Router object represents "
          + "a container for Agents, Tasks, Queues and Plans.",
      response = RouterDto.class, responseContainer = "List", tags = "routers")
  public Collection<RouterDto> list() throws CommsRouterException {

    List<RouterDto> list = routerService.list();

    LOGGER.debug("Listing all routers: {}", list);

    return list;
  }

  @GET
  @Path("{id}")
  @ApiOperation(value = "Find router by ID", notes = "Searches all routers by the given ID",
      response = RouterDto.class, tags = "routers")
  @ApiResponses({@ApiResponse(code = 200, message = "Successful operation"),
      @ApiResponse(code = 404, message = "Router with the provided id is not found",
          response = ExceptionPresentation.class)})
  public RouterDto get(
      @ApiParam(value = "ID of the router to be searched") @PathParam("id") String id)
      throws CommsRouterException {

    LOGGER.debug("Looking for router with id: {}", id);

    return routerService.get(id);
  }

  @POST
  @ApiOperation(
      value = "Create a Router",
      notes = "A Router is a container for your Tasks, Agents, Tasks, Plans and Rules.",
      response = ApiObjectId.class,
      code = 201,
      tags = "routers")
  @ApiResponses(
      @ApiResponse(code = 201, message = "Created",
          responseHeaders = @ResponseHeader(
              name = HttpHeaders.LOCATION,
              response = URL.class,
              description = "The path to the newly created resource")))
  public Response create(
      @ApiParam(value = "Router object that needs to be added to the list of routers",
          required = true) CreateRouterArg routerArg)
      throws CommsRouterException {

    LOGGER.debug("Creating router: {}", routerArg);

    ApiObjectId router = routerService.create(routerArg);

    URI createLocation =
        UriBuilder.fromResource(this.getClass()).path("{id}").build(router.getId());

    return Response.status(Status.CREATED)
        .header(HttpHeaders.LOCATION, createLocation.toString())
        .entity(new ApiObjectId(router))
        .build();
  }

  @POST
  @Path("{id}")
  @ApiOperation(value = "Update an existing router properties", tags = "routers")
  @ApiResponses({
      @ApiResponse(code = 204, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Router not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public void update(
      @ApiParam(value = "The id of the router to be updated",
          required = true) @PathParam("id") String id,
      @ApiParam(value = "UpdateRouterArg object specifying parameters to be updated",
          required = true) UpdateRouterArg routerArg)
      throws CommsRouterException {

    LOGGER.debug("Updating router: {}", routerArg);

    routerService.update(routerArg, id);
  }

  @PUT
  @Path("{id}")
  @ApiOperation(
      value = "Replace an existing router",
      notes = "If the router with the specified id does not exist, it creates it",
      tags = "routers")
  @ApiResponses({@ApiResponse(code = 200, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Router not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public Response put(
      @ApiParam(
          value = "The id of the router to be updated",
          required = true) @PathParam("id") String id,
      @ApiParam(value = "CreateRouterArg object specifying all the parameters",
          required = true) CreateRouterArg routerArg)
      throws CommsRouterException {

    LOGGER.debug("Replacing router: {}, with id: {}", routerArg, id);

    ApiObjectId router = routerService.create(routerArg, id);

    URI createLocation =
        UriBuilder.fromResource(this.getClass()).path("{id}").build(router.getId());

    return Response.status(Status.CREATED)
        .header(HttpHeaders.LOCATION, createLocation.toString())
        .entity(router)
        .build();
  }

  @DELETE
  @Path("{id}")
  @ApiOperation(value = "Deletes an existing router by ID", tags = "routers")
  @ApiResponses({
      @ApiResponse(code = 200, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Router not found",
          response = ExceptionPresentation.class)})
  public void delete(@ApiParam(value = "The id of the router to be deleted",
      required = true) @PathParam("id") String id) throws CommsRouterException {

    LOGGER.debug("Deleting router: {}", id);

    routerService.delete(id);
  }

  // Sub-resources

  @Path("{routerId}/plans")
  @ApiOperation(
      value = "Plans sub-resource",
      response = PlanResource.class,
      tags = "plans")
  public PlanResource planResource(@PathParam("routerId") String routerId) {
    LOGGER.debug("Router {} plans", routerId);

    PlanResource resource = resourceContext.getResource(PlanResource.class);
    resource.setRouterId(routerId);
    UriBuilder planResource =
        UriBuilder.fromResource(this.getClass()).path(this.getClass(), "planResource");
    resource.setEntryPoint(planResource);
    return resource;
  }

  @Path("{routerId}/queues")
  @ApiOperation(
      value = "Queues sub-resource",
      response = QueueResource.class,
      tags = "queues")
  public QueueResource queueResource(@PathParam("routerId") String routerId) {
    LOGGER.debug("Router {} queues", routerId);

    QueueResource resource = resourceContext.getResource(QueueResource.class);
    resource.setRouterId(routerId);
    UriBuilder queueResource =
        UriBuilder.fromResource(this.getClass()).path(this.getClass(), "queueResource");
    resource.setEntryPoint(queueResource);
    return resource;
  }

  @Path("{routerId}/tasks")
  @ApiOperation(
      value = "Tasks sub-resource",
      response = TaskResource.class,
      tags = "tasks")
  public TaskResource taskResource(@PathParam("routerId") String routerId) {
    LOGGER.debug("Router {} tasks", routerId);

    TaskResource resource = resourceContext.getResource(TaskResource.class);
    resource.setRouterId(routerId);
    UriBuilder taskResource =
        UriBuilder.fromResource(this.getClass()).path(this.getClass(), "taskResource");
    resource.setEntryPoint(taskResource);
    return resource;
  }

  @Path("{routerId}/agents")
  @ApiOperation(
      value = "Agents sub-resource",
      response = AgentResource.class,
      tags = "agents")
  public AgentResource agentResource(@PathParam("routerId") String routerId) {
    LOGGER.debug("Router {} agents", routerId);

    AgentResource resource = resourceContext.getResource(AgentResource.class);
    resource.setRouterId(routerId);
    UriBuilder agentResource =
        UriBuilder.fromResource(this.getClass()).path(this.getClass(), "agentResource");
    resource.setEntryPoint(agentResource);
    return resource;
  }

}
