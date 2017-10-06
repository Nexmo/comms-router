package com.softavail.commsrouter.webservice.helpers;

import com.google.common.collect.Lists;

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.webservice.mappers.ExceptionPresentation;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ResponseHeader;
import javax.validation.Valid;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.URI;
import java.util.List;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.Link;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04.09.17.
 */
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public abstract class GenericRouterObjectResource<T extends RouterObjectId>
    extends RouterObjectResource {

  private static final Logger LOGGER = LogManager.getLogger(GenericRouterObjectResource.class);

  private static final String X_TOTAL_COUNT = "X-Total-Count";

  protected abstract RouterObjectService<T> getService();

  protected URI getLocation(T routerObject) {
    return entryPoint.clone()
        .path("{resourceId}")
        .build(routerId, routerObject.getId());
  }

  protected Response createResponse(T routerObject) {
    URI createLocation = getLocation(routerObject);

    return Response.status(Status.CREATED)
        .header(HttpHeaders.LOCATION, createLocation.toString())
        .entity(new ApiObjectId(routerObject))
        .build();
  }

  protected Link[] getLinks(PaginatedList<T> pagedList) {
    List<Link> result = Lists.newArrayList();

    int pageNum = pagedList.getPage();
    int perPage = pagedList.getPerPage();
    long totalCount = pagedList.getTotalCount();
    int maxPages = Math.toIntExact((totalCount + perPage - 1) / perPage);

    // Check first
    if (pageNum > 1) {
      result.add(createLink("first", 1, perPage));
    }

    // Check prev
    if (pageNum - 1 > 0) {
      result.add(createLink("prev", pageNum - 1, perPage));
    }

    // Check next
    if (pageNum + 1 <= maxPages) {
      result.add(createLink("next", pageNum + 1, perPage));
    }

    // Check last
    if (maxPages > 1 && pageNum < maxPages) {
      result.add(createLink("last", maxPages, perPage));
    }

    return result.toArray(new Link[result.size()]);
  }

  private Link createLink(String rel, int pageNum, int perPage) {
    UriBuilder uriBuilder = entryPoint.clone()
        .queryParam("page_num", String.valueOf(pageNum));

    if (perPage != 10) {
      uriBuilder.queryParam("per_page", String.valueOf(perPage));
    }

    return Link.fromUriBuilder(uriBuilder).rel(rel).build(routerId);
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
                  name = X_TOTAL_COUNT,
                  description = "The total items from this listing",
                  response = Integer.class)}))
  public Response list(
      @ApiParam(
          value = "The current page of the listing",
          defaultValue = "1",
          allowableValues = "range[1, infinity]")
      @Valid
      @Min(value = 1L, message = "{resource.list.min.page.number}")
      @DefaultValue("01")
      @QueryParam("page_num") int pageNum,
      @ApiParam(
          value = "Number of items per page (Maximum 50)",
          defaultValue = "10",
          allowableValues = "range[1, 50]")
      @Valid
      @Min(value = 1L, message = "{resource.list.min.items.per.page}")
      @Max(value = 50, message = "{resource.list.max.items.per.page}")
      @DefaultValue("10")
      @QueryParam("per_page") int perPage)
      throws CommsRouterException {

    PaginatedList<T> pagedList = getService().listPage(routerId, pageNum, perPage);

    LOGGER.debug("Listing page {}/{} for router {}: {}",
        pageNum, perPage, routerId, pagedList);

    Link[] links = getLinks(pagedList);

    GenericEntity<List<T>> genericEntity = new GenericEntity<>(pagedList.getList(), List.class);
    return Response.ok()
        .header(X_TOTAL_COUNT, pagedList.getTotalCount())
        .links(links)
        .entity(genericEntity)
        .type(MediaType.APPLICATION_JSON_TYPE)
        .build();
  }

  @GET
  @Path("{resourceId}")
  @ApiOperation(value = "Get resource by ID", notes = "Returns resource by the given ID")
  public T get(@PathParam("resourceId") String resourceId)
      throws CommsRouterException {

    RouterObjectId routerObjectId = getRouterObjectId(resourceId);

    LOGGER.debug("Getting {}", routerObjectId);

    return getService().get(routerObjectId);
  }

  @DELETE
  @Path("{resourceId}")
  @ApiOperation(value = "Deletes an existing resource by ID")
  @ApiResponses({
      @ApiResponse(code = 200, message = "Successful operation"),
      @ApiResponse(code = 400, message = "Invalid ID supplied",
          response = ExceptionPresentation.class),
      @ApiResponse(code = 404, message = "Router not found",
          response = ExceptionPresentation.class)})
  public void delete(@PathParam("resourceId") String resourceId)
      throws CommsRouterException {

    RouterObjectId routerObjectId = getRouterObjectId(resourceId);

    LOGGER.debug("Deleting {}", routerObjectId);

    getService().delete(routerObjectId);
  }

}
