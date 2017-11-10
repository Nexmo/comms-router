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

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.dto.misc.SizeDto;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExceptionPresentation;
import com.softavail.commsrouter.api.interfaces.QueueService;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.webservice.helpers.GenericRouterObjectResource;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Collection;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
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
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Api("/queues")
public class QueueResource extends GenericRouterObjectResource<QueueDto> {

  private static final Logger LOGGER = LogManager.getLogger(QueueResource.class);

  @Inject
  private QueueService queueService;

  @Override
  protected RouterObjectService<QueueDto> getService() {
    return queueService;
  }

  @POST
  @ApiOperation(
      value = "Creates a new Queue",
      notes = "Creates a new Queue and associates it with a Router")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Successful operation",
          response = ApiObjectId.class)})
  public Response create(CreateQueueArg createArg)
      throws CommsRouterException {

    LOGGER.debug("Creating Queue {}", createArg);

    ApiObjectId queue = queueService.create(createArg, routerId);

    return createResponse(queue);
  }

  @PUT
  @Path("{resourceId}")
  @ApiOperation(
      value = "Replace an existing Queue",
      notes = "If the queue with the specified id does not exist, it creates it")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Successful operation",
          response = ApiObjectId.class),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Queue not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public Response create(
      @ApiParam(value = "The id of the queue to be replaced", required = true)
      @PathParam("resourceId")
          String resourceId,
      @ApiParam(value = "CreateQueueArg object specifying all the parameters")
          CreateQueueArg createArg)
      throws CommsRouterException {

    LOGGER.debug("Replacing queue: {}, with id: {}", createArg, resourceId);

    RouterObjectId objectId =
        RouterObjectId.builder().setId(resourceId).setRouterId(routerId).build();

    ApiObjectId queue = queueService.create(createArg, objectId);

    return createResponse(queue);
  }

  @POST
  @Path("{resourceId}")
  @ApiOperation(
      value = "Update an existing Queue",
      notes = "Modifies the Queue. If you modify a Queue and alter its predicate,"
          + " all the Agents will be evaluated and "
          + "assignments will be created and / or removed")
  @ApiResponses({
      @ApiResponse(code = 204, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Queue not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public void update(
      @ApiParam(value = "ID of the queue to be updated")
      @PathParam("resourceId")
          String resourceId,
      @ApiParam(value = "UpdateQueueArg object representing parameters "
          + "of the Queue to be updated", required = true)
          UpdateQueueArg updateArg)
      throws CommsRouterException {

    LOGGER.debug("Updating Queue {}", updateArg);

    RouterObjectId objectId =
        RouterObjectId.builder().setId(resourceId).setRouterId(routerId).build();

    queueService.update(updateArg, objectId);
  }

  @GET
  @Path("{resourceId}/size")
  @ApiOperation(
      value = "Get the size of the Queue",
      notes = "Returns the number of Tasks in waiting state in "
          + "the Queue identified by {resourceId}")
  @ApiResponses({
      @ApiResponse(code = 200, message = "Successful operation",
          response = SizeDto.class)})
  public SizeDto count(@PathParam("resourceId") String resourceId)
      throws CommsRouterException {

    long queueSize = queueService.getQueueSize(getRouterObjectId(resourceId));

    return new SizeDto(queueSize);
  }

  @GET
  @Path("{resourceId}/tasks")
  @ApiOperation(
      value = "Get the Tasks waiting in the Queue",
      notes = "List all Tasks in waiting state in the Queue identified by {resourceId}")
  @ApiResponses({
      @ApiResponse(code = 200, message = "Successful operation",
          response = TaskDto.class, responseContainer = "List")})
  public Collection<TaskDto> getTasks(@PathParam("resourceId") String resourceId)
      throws CommsRouterException {

    return queueService.getTasks(getRouterObjectId(resourceId));
  }

}
