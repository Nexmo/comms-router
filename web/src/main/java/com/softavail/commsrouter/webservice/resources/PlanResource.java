package com.softavail.commsrouter.webservice.resources;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.PlanService;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.webservice.helpers.GenericRouterObjectResource;
import com.softavail.commsrouter.webservice.mappers.ExceptionPresentation;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
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
@Api("/plans")
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class PlanResource extends GenericRouterObjectResource<PlanDto> {

  private static final Logger LOGGER = LogManager.getLogger(PlanResource.class);

  @Inject
  private PlanService planService;

  @Override
  protected RouterObjectService<PlanDto> getService() {
    return planService;
  }

  @POST
  @ApiOperation(value = "Add new Plan", notes = "Add new Plan and associate it with a Router",
      response = ApiObject.class)
  public Response create(CreatePlanArg planArg) throws CommsRouterException {

    LOGGER.debug("Creating plan {}", planArg);

    RouterObjectId objectId = RouterObjectId.builder().setRouterId(routerId).build();

    PlanDto plan = planService.create(planArg, objectId);

    return createResponse(plan);
  }

  @POST
  @Path("{resourceId}")
  @ApiOperation(value = "Update an existing Plan",
      notes = "Update some properties of an existing Plan", tags = "plans")
  @ApiResponses({@ApiResponse(code = 200, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Plan not found", response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public void update(
      @ApiParam(value = "ID of the plan to be updated") @PathParam("resourceId") String resourceId,
      @ApiParam(value = "UpdatePlanArg object representing parameters of the Plan to be updated",
          required = true) UpdatePlanArg planArg)
      throws CommsRouterException {

    LOGGER.debug("Updating plan {}", planArg);

    RouterObjectId objectId =
        RouterObjectId.builder().setId(resourceId).setRouterId(routerId).build();

    planService.update(planArg, objectId);
  }

  @PUT
  @Path("{resourceId}")
  @ApiOperation(value = "Replace an existing Plan",
      notes = "If the plan with the specified id does not exist, it creates it", tags = "plans")
  @ApiResponses({@ApiResponse(code = 200, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Plan not found", response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public Response put(
      @ApiParam(value = "The id of the plan to be replaced",
          required = true) @PathParam("resourceId") String resourceId,
      @ApiParam(
          value = "CreatePlanArg object specifying all the parameters") CreatePlanArg createArg)
      throws CommsRouterException {

    LOGGER.debug("Replacing plan: {}, with id: {}", createArg, resourceId);

    RouterObjectId objectId =
        RouterObjectId.builder().setId(resourceId).setRouterId(routerId).build();

    PlanDto plan = planService.put(createArg, objectId);

    return createResponse(plan);
  }


}
