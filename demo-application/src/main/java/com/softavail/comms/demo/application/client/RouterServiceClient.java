package com.softavail.comms.demo.application.client;

import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;
import com.softavail.commsrouter.api.exception.NotFoundException;

import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.UriBuilder;
import com.softavail.commsrouter.api.interfaces.RouterService;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class RouterServiceClient extends ServiceClientBase<RouterDto>
    implements RouterService {

  private Client client;

  private Configuration configuration;

  @Inject
  public RouterServiceClient(Client client, Configuration configuration) {
    super(RouterDto.class);
    this.client = client;
    this.configuration = configuration;
  }

  @Override
  UriBuilder getApiUrl() {
    return UriBuilder
        .fromPath(configuration.getCommsApiEndpoint())
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
  public void update(UpdateRouterArg updateArg)
      throws NotFoundException {

    put(updateArg);
  }

  @Override
  public RouterDto get(String id) throws NotFoundException {
    return getItem(id);
  }

  @Override
  public List<RouterDto> list() {
    return getList();
  }

  @Override
  public void delete(String id) {
    super.delete(id);
  }

}
