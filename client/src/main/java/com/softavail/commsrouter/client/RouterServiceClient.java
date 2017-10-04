package com.softavail.commsrouter.client;

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.RouterService;

import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class RouterServiceClient extends ServiceClientBase<RouterDto>
    implements RouterService {

  private Client client;
  private final String endpoint;

  @Inject
  public RouterServiceClient(Client client, String endpoint) {
    super(RouterDto.class);
    this.client = client;
    this.endpoint = endpoint;
  }

  @Override
  UriBuilder getApiUrl() {
    return UriBuilder
        .fromPath(endpoint)
        .path("routers").clone();
  }

  @Override
  Client getClient() {
    return client;
  }

  @Override
  public RouterDto create(CreateRouterArg createArg) {
    return post(createArg);
  }

  @Override
  public void update(UpdateRouterArg updateArg, ApiObjectId id)
      throws NotFoundException {

    post(updateArg, id);
  }

  @Override
  public RouterDto get(String id)
      throws NotFoundException {

    return getItem(new ApiObjectId(id));
  }

  @Override
  public RouterDto replace(CreateRouterArg createArg, ApiObjectId objectId)
      throws CommsRouterException {

    return put(createArg, objectId);
  }

  @Override
  public List<RouterDto> list() {
    return getList();
  }

  @Override
  public void delete(String id) {
    deleteRequest(new ApiObjectId(id));
  }

}
