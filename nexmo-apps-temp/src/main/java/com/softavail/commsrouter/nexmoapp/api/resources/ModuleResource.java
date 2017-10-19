package com.softavail.commsrouter.nexmoapp.api.resources;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExceptionPresentation;
import com.softavail.commsrouter.api.interfaces.PaginatedService;
import com.softavail.commsrouter.nexmoapp.dto.arg.CreateExpressionArg;
import com.softavail.commsrouter.nexmoapp.dto.arg.UpdateExpressionArg;
import com.softavail.commsrouter.nexmoapp.dto.model.ModuleDto;
import com.softavail.commsrouter.nexmoapp.interfaces.ModuleService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ResponseHeader;

import java.net.URL;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
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
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 10.10.17.
 */
@Api
@Path("expression")
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class ModuleResource extends ApiObjectResource<ModuleDto> {

  @Context
  private ResourceContext resourceContext;

  @Inject
  private ModuleService moduleService;

  @Override
  protected PaginatedService<ModuleDto> getService() {
    return moduleService;
  }

  @Override
  protected UriBuilder getEntryPoint() {
    return UriBuilder.fromResource(this.getClass());
  }

  @POST
  @ApiOperation(
      value = "Create Expression",
      notes = "Expression works over incoming call.")
  @ApiResponses(
      @ApiResponse(
          code = 201,
          message = "Created",
          response = ApiObjectId.class,
          responseHeaders = @ResponseHeader(
              name = HttpHeaders.LOCATION,
              response = URL.class,
              description = "The path to the newly created resource")))
  public Response create(CreateExpressionArg arg)
      throws CommsRouterException {

    ApiObjectId apiObjectId = moduleService.create(arg);
    return createResponse(apiObjectId);
  }

  @PUT
  @Path("{resourceId}")
  @ApiOperation(
      value = "Create or Replace existing Expression",
      notes = "Creates Expression with the specified ID or replaces the existing one")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Created",
          response = ApiObjectId.class,
          responseHeaders = @ResponseHeader(
              name = HttpHeaders.LOCATION,
              response = URL.class,
              description = "The path to the newly created resource")),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public Response create(@PathParam("resourceId") String resourceId, CreateExpressionArg arg)
      throws CommsRouterException {

    ApiObjectId apiObjectId = moduleService.create(arg, resourceId);
    return createResponse(apiObjectId);
  }

  @POST
  @Path("{resourceId}")
  @ApiOperation("Update an existing Expression")
  @ApiResponses({
      @ApiResponse(code = 204, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public void update(@PathParam("resourceId") String resourceId, UpdateExpressionArg arg)
      throws CommsRouterException {

    moduleService.update(arg, resourceId);
  }

}
