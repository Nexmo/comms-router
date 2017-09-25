package com.softavail.commsrouter.webservice.resources;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.QueueService;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.webservice.helpers.GenericRouterObjectResource;
import com.softavail.commsrouter.webservice.model.SizeWrapper;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
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
      notes = "Creates a new Queue and associates it with a Router",
      response = ApiObject.class)
  public Response create(CreateQueueArg createArg)
      throws CommsRouterException {

    createArg.setRouterId(routerId);

    LOGGER.debug("Creating Queue {}", createArg);

    QueueDto queue = queueService.create(createArg);

    return createResponse(queue);
  }

  @PUT
  @Path("{resourceId}")
  @ApiOperation(
      value = "Update an existing Queue",
      notes = "Modifies the Queue. If you modify a Queue and alter its predicate,"
          + " all the Agents will be evaluated and assignments will be created and / or removed")
  public void update(@PathParam("resourceId") String resourceId, UpdateQueueArg updateArg)
      throws CommsRouterException {

    updateArg.setRouterId(routerId);
    updateArg.setId(resourceId);

    LOGGER.debug("Updating Queue {}", updateArg);

    queueService.update(updateArg);
  }

  @GET
  @Path("{resourceId}/size")
  @ApiOperation(
      value = "Get the size of the Queue",
      notes = "Returns the number of Tasks in waiting state in the Queue identified by {resourceId}",
      response = SizeWrapper.class)
  public SizeWrapper count(@PathParam("resourceId") String resourceId)
      throws CommsRouterException {

    long queueSize = queueService.getQueueSize(getRouterObject(resourceId));

    return new SizeWrapper(queueSize);
  }

  @GET
  @Path("{resourceId}/tasks")
  @ApiOperation(
      value = "Get the Tasks waiting in the Queue",
      notes = "List all Tasks in waiting state in the Queue identified by {resourceId}",
      response = TaskDto.class,
      responseContainer = "List")
  public Collection<TaskDto> getTasks(@PathParam("resourceId") String resourceId)
      throws CommsRouterException {

    return queueService.getTasks(getRouterObject(resourceId));
  }

}
