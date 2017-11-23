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

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExceptionPresentation;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.api.interfaces.TaskService;
import com.softavail.commsrouter.webservice.helpers.GenericRouterObjectResource;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ResponseHeader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.URL;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.container.ResourceContext;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;

/**
 * Created by @author mapuo on 31.08.17.
 */
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Api("/tasks")
public class TaskResource extends GenericRouterObjectResource<TaskDto> {

  private static final Logger LOGGER = LogManager.getLogger(TaskResource.class);

  @Inject
  private TaskService taskService;

  @Context
  private ResourceContext resourceContext;

  @Override
  protected RouterObjectService<TaskDto> getService() {
    return taskService;
  }

  protected Response createResponse(CreatedTaskDto task) {
    ResponseBuilder builder = Response.status(Status.CREATED)
        .header(HttpHeaders.LOCATION, getLocation(task).toString())
        .header(TaskService.X_QUEUE_SIZE, task.getQueueTasks())
        .entity(task)
        .type(MediaType.APPLICATION_JSON_TYPE);

    return builder.build();
  }

  @GET
  @Path("byTag")
  @ApiOperation(value = "Get resource by Tag", notes = "Returns resource by the given Tag")
  public Response getByTag(@QueryParam("tag") String tag)
      throws CommsRouterException {

    LOGGER.debug("Getting by Tag {}", tag);

    TaskDto taskDto = taskService.getByTag(routerRef, tag);

    return Response.ok(taskDto, MediaType.APPLICATION_JSON_TYPE)
        .build();
  }

  @POST
  @ApiOperation(
      value = "Add new Task",
      notes = "Create a new Task within a Router")
  @ApiResponses(
      @ApiResponse(
          code = 201,
          message = "Created successfully",
          response = CreatedTaskDto.class,
          responseHeaders = {
              @ResponseHeader(
                  name = HttpHeaders.LOCATION,
                  response = URL.class,
                  description = "The path to the newly created resource"),
              @ResponseHeader(
                  name = TaskService.X_QUEUE_SIZE,
                  response = Long.class,
                  description = "The number of tasks in the queue before that one")}))
  public Response create(CreateTaskArg taskArg)
      throws CommsRouterException {

    LOGGER.debug("Creating Task: {}", taskArg);

    CreatedTaskDto task = taskService.create(taskArg, routerRef);

    return createResponse(task);
  }

  @PUT
  @Path("{resourceId}")
  @ApiOperation(
      value = "Replace an existing Task",
      notes = "If the task with the specified id does not exist, it creates it")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Successful operation",
          response = CreatedTaskDto.class),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Task not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public Response create(
      @ApiParam(value = "The id of the task to be replaced", required = true)
      @PathParam("resourceId")
          String resourceId,
      @ApiParam("CreateTaskArg object specifying all the parameters")
          CreateTaskArg taskArg)
      throws CommsRouterException {

    LOGGER.debug("Replacing task: {}, with id: {}", taskArg, resourceId);

    RouterObjectRef objectId = getRouterObjectRef(resourceId);

    CreatedTaskDto task = taskService.replace(taskArg, objectId);

    return createResponse(task);
  }

  @POST
  @Path("{resourceId}")
  @ApiOperation(
      value = "Update an existing Task",
      notes = "Update some properties of an existing Task")
  @ApiResponses({
      @ApiResponse(code = 204, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Task not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public void update(@PathParam("resourceId") String resourceId, UpdateTaskArg taskArg)
      throws CommsRouterException {

    RouterObjectRef objectId = getRouterObjectRef(resourceId);

    LOGGER.debug("Updating task: {}", taskArg);

    taskService.update(taskArg, objectId);
  }

  // Sub-resources

  @Path("{resourceId}/user_context")
  @ApiOperation(value = "/user_context", response = UserContextResource.class)
  public UserContextResource userContextResource(@PathParam("resourceId") String resourceId) {
    LOGGER.debug("Router {} Task {} Context", routerRef, resourceId);

    RouterObjectRef routerObjectId = getRouterObjectRef(resourceId);

    UserContextResource resource = resourceContext.getResource(UserContextResource.class);
    resource.setRouterObject(routerObjectId);

    return resource;
  }

}
