package com.softavail.commsrouter.webservice.resources;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.PlanService;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
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
  @ApiOperation(
      value = "Add new Plan",
      notes = "Add new Plan and associate it with a Router",
      response = ApiObject.class)
  public Response create(CreatePlanArg planArg) throws CommsRouterException {
    planArg.setRouterId(routerId);

    LOGGER.debug("Creating plan {}", planArg);

    PlanDto plan = planService.create(planArg);

    return createResponse(plan);
  }

  @PUT
  @Path("{resourceId}")
  @ApiOperation(
      value = "Update an existing Plan",
      notes = "Update some properties of an existing Plan")
  public void update(@PathParam("resourceId") String resourceId, UpdatePlanArg planArg)
      throws CommsRouterException {

    planArg.setRouterId(routerId);
    planArg.setId(resourceId);

    LOGGER.debug("Updating plan {}", planArg);

    planService.update(planArg);
  }


}
