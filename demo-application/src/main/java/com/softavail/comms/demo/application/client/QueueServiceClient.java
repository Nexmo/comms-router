package com.softavail.comms.demo.application.client;

import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.exception.NotFoundException;

import java.util.Collection;
import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.UriBuilder;
import com.softavail.commsrouter.api.interfaces.QueueService;

/**
 * Created by @author mapuo on 05.09.17.
 */
public class QueueServiceClient extends ServiceClientBase<QueueDto>
    implements QueueService {

  private Client client;

  private Configuration configuration;

  @Inject
  public QueueServiceClient(Client client, Configuration configuration) {
    super(QueueDto.class);
    this.client = client;
    this.configuration = configuration;
  }

  @Override
  UriBuilder getApiUrl() {
    return UriBuilder
        .fromPath(configuration.getCommsApiEndpoint())
        .path("routers")
        .path("{routerId}")
        .path("queues")
        .clone();
  }

  @Override
  Client getClient() {
    return client;
  }

  @Override
  public QueueDto get(RouterObject routerObject)
      throws NotFoundException {

    return getItem(routerObject);
  }

  @Override
  public List<QueueDto> list(String routerId) {
    return getList(routerId);
  }

  @Override
  public PaginatedList<QueueDto> listPage(String routerId, int page, int perPage) {
    return getList(routerId, page, perPage);
  }

  @Override
  public void delete(RouterObject routerObject) {
    routerObject.setRouterId(configuration.getCommsRouterId());
    super.delete(routerObject);
  }

  @Override
  public QueueDto create(CreateQueueArg createArg)
      throws NotFoundException {

    createArg.setRouterId(configuration.getCommsRouterId());
    return post(createArg);
  }

  @Override
  public void update(UpdateQueueArg updateArg)
      throws NotFoundException {

    updateArg.setRouterId(configuration.getCommsRouterId());

    put(updateArg);
  }

  @Override
  public long getQueueSize(RouterObject routerObjectId) throws NotFoundException {
    return 0; // TODO
  }

  @Override
  public Collection<TaskDto> getTasks(RouterObject routerObjectId) throws NotFoundException {
    return null; // TODO
  }
}
