/*
 * Copyright 2017 - 2018 SoftAvail Inc.
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

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.skill.EnumerationSkillDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberInterval;
import com.softavail.commsrouter.api.dto.model.skill.NumberIntervalBoundary;
import com.softavail.commsrouter.api.dto.model.skill.NumberSkillDto;
import com.softavail.commsrouter.api.dto.model.skill.SkillDto;
import com.softavail.commsrouter.api.dto.model.skill.StringSkillDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExceptionPresentation;
import com.softavail.commsrouter.api.interfaces.PaginatedService;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.api.interfaces.SkillService;
import com.softavail.commsrouter.api.service.PaginationHelper;
import com.softavail.commsrouter.webservice.helpers.GenericRouterObjectResource;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ResponseHeader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Pattern;
import javax.ws.rs.Consumes;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 *
 * @author ikrustev
 */
@Api("/skills")
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class SkillResource extends GenericRouterObjectResource<SkillDto> {

  private static final Logger LOGGER = LogManager.getLogger(SkillResource.class);

  // @Inject
  private SkillService service;

  @Override
  protected RouterObjectService<SkillDto> getService() {
    return service;
  }

  @GET
  @ApiOperation(
      value = "List all resources",
      notes = "Default paging will be applied",
      responseContainer = "List")
  @ApiResponses(
      @ApiResponse(
          code = 200,
          message = "Successful operation",
          responseHeaders = {
              @ResponseHeader(
                  name = PaginatedService.NEXT_TOKEN_HEADER,
                  description = "The token for the next page",
                  response = String.class)}))
  public Response list(
      @ApiParam(
          value = "The token from the previous request",
          defaultValue = "")
      @Valid
      @QueryParam(PaginatedService.TOKEN_PARAM) String token,
      @ApiParam(
          value = "Number of items per page (Maximum 50)",
          defaultValue = "10",
          allowableValues = "range[1, 50]")
      @Valid
      @Min(value = 1L, message = "{resource.list.min.items.per.page}")
      @Max(value = 50, message = "{resource.list.max.items.per.page}")
      @DefaultValue("10")
      @QueryParam(PaginatedService.ITEMS_PER_PAGE_PARAM) int perPage,
      @Valid
      @Pattern(regexp = PaginationHelper.SORT_REGEX, message = "{resource.list.sort.message}")
      @QueryParam("sort") String sort,
      @QueryParam("q") String query)
      throws CommsRouterException {

    PagingRequest pagingRequest = new PagingRequest(routerRef, token, perPage, sort, query);

    List<SkillDto> list = new ArrayList<>();

    EnumerationSkillDto enumerationSkill;

    enumerationSkill = new EnumerationSkillDto();
    enumerationSkill.setRouterRef(routerRef);
    enumerationSkill.setRef("language");
    enumerationSkill.setDescription("Representes the set of languages spoken by an agent");
    enumerationSkill.setMultivalue(true);
    enumerationSkill.setValues(Arrays.asList("en", "es"));
    list.add(enumerationSkill);

    enumerationSkill = new EnumerationSkillDto();
    enumerationSkill.setRouterRef(routerRef);
    enumerationSkill.setRef("service");
    enumerationSkill.setDescription("Representes the set of services that an agent could be capable of");
    enumerationSkill.setMultivalue(true);
    enumerationSkill.setValues(Arrays.asList("support", "sale", "help"));
    list.add(enumerationSkill);

    NumberSkillDto numberSkill;

    numberSkill = new NumberSkillDto();
    numberSkill.setRouterRef(routerRef);
    numberSkill.setRef("age");
    numberSkill.setDescription("Representes the age of an agent");
    numberSkill.setMultivalue(false);

    NumberInterval teens = new NumberInterval();
    teens.setLow(new NumberIntervalBoundary(13.0, true));
    teens.setHigh(new NumberIntervalBoundary(20.0, false));

    NumberInterval seniors = new NumberInterval();
    seniors.setLow(new NumberIntervalBoundary(60.0, Boolean.FALSE));
    seniors.setHigh(NumberIntervalBoundary.POSITIVE_INFINITY);

    numberSkill.setIntervals(Arrays.asList(teens, seniors));
    list.add(numberSkill);

    StringSkillDto stringSkill;

    stringSkill = new StringSkillDto();
    stringSkill.setRouterRef(routerRef);
    stringSkill.setRef("external-agent-id");
    stringSkill.setDescription("Representes some external ID of an agent and can be used by the task to express preference toward particular agent");
    stringSkill.setMultivalue(false);
    stringSkill.setRegexp("id-.+");
    list.add(stringSkill);

    PaginatedList<SkillDto> pagedList = new PaginatedList<>(list, "next-page-token");

    LOGGER.debug("Listing page {}/{} for router {}: {}",
        token, perPage, routerRef, pagedList);

    GenericEntity<List<SkillDto>> genericEntity = new GenericEntity<>(pagedList.getList(), List.class);
    return Response.ok()
        .header(PaginatedService.NEXT_TOKEN_HEADER, pagedList.getNextToken())
        .entity(genericEntity)
        .type(MediaType.APPLICATION_JSON_TYPE)
        .build();
  }

  @GET
  @Path("{resourceRef}")
  @ApiOperation(value = "Get resource by ID", notes = "Returns resource by the given ID")
  public SkillDto get(@PathParam("resourceRef") String resourceRef)
      throws CommsRouterException {

    RouterObjectRef routerObjectId = getRouterObjectRef(resourceRef);

    LOGGER.debug("Getting {}", routerObjectId);

    return getService().get(routerObjectId);
  }

  @POST
  @ApiOperation(
      value = "Add new Skill",
      notes = "Add new Skill and associate it with a Router")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Successful operation", response = ApiObjectRef.class)})
  public Response create(CreateAgentArg agentArg) throws CommsRouterException {

    LOGGER.debug("Creating skill {}", agentArg);

    ApiObjectRef agent = service.create(agentArg, routerRef);

    return createResponse(agent);
  }

  @PUT
  @Path("{resourceId}")
  @ApiOperation(
      value = "Replace an existing Agent",
      notes = "If the agent with the specified id does not exist, it creates it")
  @ApiResponses({
      @ApiResponse(code = 201, message = "Successful operation", response = ApiObjectRef.class),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Agent not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public Response create(
      @ApiParam(value = "The id of the agent to be replaced", required = true)
      @PathParam("resourceId")
          String resourceId,
      @ApiParam(value = "CreateAgentArg object specifying all the parameters")
          CreateAgentArg agentArg)
      throws CommsRouterException {

    LOGGER.debug("Replacing agent: {}, with id: {}", agentArg, resourceId);

    RouterObjectRef objectRef =
        RouterObjectRef.builder().setRef(resourceId).setRouterRef(routerRef).build();

    ApiObjectRef agent = service.replace(agentArg, objectRef);

    return createResponse(agent);
  }

  @POST
  @Path("{resourceId}")
  @ApiOperation(
      value = "Update an existing Agent",
      notes = "Update some properties of an existing Agent")
  @ApiResponses({
      @ApiResponse(code = 204, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Agent not found",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 405, message = "Validation exception",
          response = ExceptionPresentation.class)})
  public void update(
      @ApiParam(value = "ID of the agent to be updated")
      @PathParam("resourceId")
          String resourceId,
      @ApiParam(
          value = "UpdateAgentArg object representing parameters of the Agent to be updated",
          required = true)
          UpdateAgentArg agentArg)
      throws CommsRouterException {

    LOGGER.debug("Updating agent {}", agentArg);

    RouterObjectRef objectId =
        RouterObjectRef.builder().setRef(resourceId).setRouterRef(routerRef).build();

    service.update(agentArg, objectId);
  }

}
