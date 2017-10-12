package com.softavail.commsrouter.webservice.resources;

import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.TaskService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 * Created by @author mapuo on 18.09.17.
 */
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class UserContextResource {

  private static final Logger LOGGER = LogManager.getLogger(UserContextResource.class);

  @Inject
  TaskService taskService;

  private RouterObjectId routerObjectId;

  public void setRouterObject(RouterObjectId routerObjectId) {
    this.routerObjectId = routerObjectId;
  }

  @GET
  @ApiOperation(
      value = "Get the current user context",
      response = AttributeGroupDto.class)
  public Response getContext()
      throws CommsRouterException {

    TaskDto taskDto = taskService.get(routerObjectId);

    return Response.ok(taskDto.getUserContext()).build();
  }

  @GET
  @Path("{key}")
  @ApiOperation(
      value = "Get the value with specified key from user context",
      response = AttributeValueDto.class)
  public Response getUserContextKey(@PathParam("key") String key)
      throws CommsRouterException {

    TaskDto taskDto = taskService.get(routerObjectId);

    return Response.ok(taskDto.getUserContext().get(key)).build();
  }

  @PUT
  @ApiOperation("Update the whole user context")
  @ApiResponses(
      @ApiResponse(code = 204, message = "Successful operation"))
  public void replaceContext(UpdateTaskContext taskContext)
      throws CommsRouterException {

    taskService.update(taskContext, routerObjectId);
  }

  @PUT
  @Path("{key}")
  @ApiOperation("Create/Update a value with specified key in user context")
  @ApiResponses(
      @ApiResponse(code = 204, message = "Successful operation"))
  public void updateKey(@PathParam("key") String key, AttributeValueDto valueDto)
      throws CommsRouterException {

    TaskDto taskDto = taskService.get(routerObjectId);
    taskDto.getUserContext().put(key, valueDto);

    UpdateTaskContext context = new UpdateTaskContext();
    context.setUserContext(taskDto.getUserContext());

    taskService.update(context, routerObjectId);
  }

}
