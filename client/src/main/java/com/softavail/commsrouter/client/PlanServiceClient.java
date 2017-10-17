package com.softavail.commsrouter.client;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.PlanService;

import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class PlanServiceClient extends ServiceClientBase<PlanDto, ApiObjectId>
    implements PlanService {

  private final Client client;
  private final String endpoint;
  private final String routerId;

  @Inject
  public PlanServiceClient(Client client, String endpoint, String routerId) {
    this.client = client;
    this.endpoint = endpoint;
    this.routerId = routerId;
  }

  @Override
  UriBuilder getApiUrl() {
    return UriBuilder.fromPath(endpoint)
        .path("routers")
        .path("{routerId}")
        .path("plans")
        .clone();
  }

  @Override
  Client getClient() {
    return client;
  }

  @Override
  public PlanDto get(RouterObjectId routerObject)
      throws NotFoundException {

    return getItem(new RouterObjectId(routerObject.getId(), routerObject.getRouterId()));
  }

  @Override
  public List<PlanDto> list(String routerId) {
    return getList(routerId);
  }

  @Override
  public PaginatedList<PlanDto> list(String routerId, int page, int perPage) {
    return getList(routerId, page, perPage);
  }

  @Override
  public void delete(RouterObjectId routerObject) {
    routerObject.setRouterId(routerId);
    deleteRequest(new RouterObjectId(routerObject.getId(), routerObject.getRouterId()));
  }

  @Override
  public ApiObjectId create(CreatePlanArg createArg, String id) {

    return post(createArg, id);
  }

  @Override
  public ApiObjectId create(CreatePlanArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    return put(createArg, objectId);
  }

  @Override
  public void update(UpdatePlanArg updateArg, RouterObjectId id)
      throws NotFoundException {

    post(updateArg, id);
  }

}
