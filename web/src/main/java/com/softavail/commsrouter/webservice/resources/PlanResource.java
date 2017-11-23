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

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExceptionPresentation;
import com.softavail.commsrouter.api.interfaces.PlanService;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.webservice.helpers.GenericRouterObjectResource;
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
  @ApiOperation(
      value = "Add new Plan",
      notes = "Add new Plan and associate it with a Router")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Successful operation", response = ApiObjectRef.class)})
  public Response create(CreatePlanArg planArg) throws CommsRouterException {

    LOGGER.debug("Creating plan {}", planArg);

    ApiObjectRef plan = planService.create(planArg, routerRef);

    return createResponse(plan);
  }

  @PUT
  @Path("{resourceId}")
  @ApiOperation(
      value = "Replace an existing Plan",
      notes = "If the plan with the specified id does not exist, it creates it")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Successful operation", response = ApiObjectRef.class),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Plan not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public Response create(
      @ApiParam(value = "The id of the plan to be replaced", required = true)
      @PathParam("resourceId")
          String resourceId,
      @ApiParam(value = "CreatePlanArg object specifying all the parameters")
          CreatePlanArg createArg)
      throws CommsRouterException {

    LOGGER.debug("Replacing plan: {}, with id: {}", createArg, resourceId);

    RouterObjectRef objectRef =
        RouterObjectRef.builder().setRef(resourceId).setRouterRef(routerRef).build();

    ApiObjectRef plan = planService.replace(createArg, objectRef);

    return createResponse(plan);
  }

  @POST
  @Path("{resourceId}")
  @ApiOperation(
      value = "Update an existing Plan",
      notes = "Update some properties of an existing Plan")
  @ApiResponses({
      @ApiResponse(code = 204, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Plan not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public void update(
      @ApiParam(value = "ID of the plan to be updated") @PathParam("resourceId") String resourceId,
      @ApiParam(value = "UpdatePlanArg object representing parameters of the Plan to be updated",
          required = true) UpdatePlanArg planArg)
      throws CommsRouterException {

    LOGGER.debug("Updating plan {}", planArg);

    RouterObjectRef objectId =
        RouterObjectRef.builder().setRef(resourceId).setRouterRef(routerRef).build();

    planService.update(planArg, objectId);
  }

}
