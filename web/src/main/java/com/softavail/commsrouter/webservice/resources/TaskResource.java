package com.softavail.commsrouter.webservice.resources;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.api.interfaces.TaskService;
import com.softavail.commsrouter.webservice.helpers.GenericRouterObjectResource;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.container.ResourceContext;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 * Created by @author mapuo on 31.08.17.
 */
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Api("/tasks")
public class TaskResource extends GenericRouterObjectResource<TaskDto> {

  private static final Logger LOGGER = LogManager.getLogger(TaskResource.class);

  @Inject
  TaskService taskService;

  @Context
  private ResourceContext resourceContext;

  @Override
  protected RouterObjectService<TaskDto> getService() {
    return taskService;
  }

  @POST
  @ApiOperation(
      value = "Add new Task",
      notes = "Add new Task and associate it with a Router",
      response = ApiObject.class)
  public Response create(CreateTaskArg taskArg)
      throws CommsRouterException {

    taskArg.setRouterId(routerId);

    LOGGER.debug("Creating Task: {}", taskArg);

    TaskDto task = taskService.create(taskArg);

    return createResponse(task);
  }

  @PUT
  @Path("{resourceId}")
  @ApiOperation(
      value = "Update an existing Task",
      notes = "Update some properties of an existing Task")
  public void update(@PathParam("resourceId") String resourceId, UpdateTaskArg taskArg)
      throws CommsRouterException {

    taskArg.setRouterId(routerId);
    taskArg.setId(resourceId);

    LOGGER.debug("Updating task: {}", taskArg);

    taskService.update(taskArg);
  }

  // Sub-resources

  @Path("{resourceId}/user_context")
  @ApiOperation(value = "/user_context", response = UserContextResource.class)
  public UserContextResource userContextResource(@PathParam("resourceId") String resourceId) {
    LOGGER.debug("Router {} Task {} Context", routerId, resourceId);

    RouterObject routerObject = getRouterObject(resourceId);

    UserContextResource resource = resourceContext.getResource(UserContextResource.class);
    resource.setRouterObject(routerObject);

    return resource;
  }

}
