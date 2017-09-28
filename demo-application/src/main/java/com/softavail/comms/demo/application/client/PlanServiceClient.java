package com.softavail.comms.demo.application.client;

import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.UriBuilder;
import com.softavail.commsrouter.api.interfaces.PlanService;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class PlanServiceClient extends ServiceClientBase<PlanDto> implements PlanService {

  private Client client;

  private Configuration configuration;

  @Inject
  public PlanServiceClient(Client client, Configuration configuration) {
    super(PlanDto.class);
    this.client = client;
    this.configuration = configuration;
  }

  @Override
  UriBuilder getApiUrl() {
    return UriBuilder.fromPath(configuration.getCommsApiEndpoint()).path("routers")
        .path("{routerId}").path("plans").clone();
  }

  @Override
  Client getClient() {
    return client;
  }

  @Override
  public PlanDto get(RouterObjectId routerObject) throws NotFoundException {

    return getItem(new RouterObjectId(routerObject.getId(), routerObject.getRouterId()));
  }

  @Override
  public List<PlanDto> list(String routerId) {
    return getList(routerId);
  }

  @Override
  public PaginatedList<PlanDto> listPage(String routerId, int page, int perPage) {
    return getList(routerId, page, perPage);
  }

  @Override
  public void delete(RouterObjectId routerObject) {
    routerObject.setRouterId(configuration.getCommsRouterId());
    super.delete(new RouterObjectId(routerObject.getId(), routerObject.getRouterId()));
  }

  @Override
  public PlanDto create(CreatePlanArg createArg, RouterObjectId id) {

    return post(createArg, id.getRouterId());
  }

  @Override
  public void update(UpdatePlanArg updateArg, RouterObjectId id) throws NotFoundException {

    post(updateArg, id);
  }

  @Override
  public PlanDto put(CreatePlanArg createArg, RouterObjectId objectId) throws CommsRouterException {

    return put(createArg, objectId);
  }

}
