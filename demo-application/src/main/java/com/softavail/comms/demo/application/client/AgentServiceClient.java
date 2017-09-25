package com.softavail.comms.demo.application.client;

import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.exception.NotFoundException;
import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.UriBuilder;
import com.softavail.commsrouter.api.interfaces.AgentService;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class AgentServiceClient extends ServiceClientBase<AgentDto>
    implements AgentService {

  private Client client;

  private Configuration configuration;

  @Inject
  public AgentServiceClient(Client client, Configuration configuration) {
    super(AgentDto.class);
    this.client = client;
    this.configuration = configuration;
  }

  @Override
  UriBuilder getApiUrl() {
    return UriBuilder
        .fromPath(configuration.getCommsApiEndpoint())
        .path("routers")
        .path("{routerId}")
        .path("agents")
        .clone();
  }

  @Override
  Client getClient() {
    return client;
  }

  @Override
  public AgentDto get(RouterObject routerObjectId) throws NotFoundException {
    return null;
  }

  @Override
  public List<AgentDto> list(String routerId) {
    return null;
  }

  @Override
  public PaginatedList<AgentDto> listPage(String routerId, int page, int perPage) {
    return null;
  }

  @Override
  public void delete(RouterObject routerObjectId) {

  }

  @Override
  public AgentDto create(CreateAgentArg createArg) throws NotFoundException {
    return null;
  }

  @Override
  public void update(UpdateAgentArg updateArg) throws NotFoundException {

  }

}
