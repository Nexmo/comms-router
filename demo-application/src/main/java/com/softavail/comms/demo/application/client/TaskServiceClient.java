package com.softavail.comms.demo.application.client;

import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;

import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.UriBuilder;
import com.softavail.commsrouter.api.interfaces.TaskService;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class TaskServiceClient extends ServiceClientBase<TaskDto> implements TaskService {

  private Client client;

  private Configuration configuration;

  @Inject
  public TaskServiceClient(Client client, Configuration configuration) {
    super(TaskDto.class);
    this.client = client;
    this.configuration = configuration;
  }

  @Override
  UriBuilder getApiUrl() {
    return UriBuilder.fromPath(configuration.getCommsApiEndpoint()).path("routers")
        .path("{routerId}").path("tasks").clone();
  }

  @Override
  Client getClient() {
    return client;
  }

  @Override
  public TaskDto create(CreateTaskArg createArg, RouterObjectId id) throws CommsRouterException {

    // post on container, creates object with auto generated id
    return post(createArg, id.getRouterId());
  }

  @Override
  public void update(UpdateTaskArg updateArg, RouterObjectId id) throws CommsRouterException {

    // post on resource, updates it with parameters provided 
    post(updateArg, id);
  }

  @Override
  public void update(UpdateTaskContext taskContext) throws CommsRouterException {
    taskContext.setRouterId(configuration.getCommsRouterId());

    post(taskContext, new RouterObjectId(taskContext.getId(), configuration.getCommsRouterId()));
  }

  @Override
  public TaskDto put(CreateTaskArg createArg, RouterObjectId objectId) throws CommsRouterException {

    return put(createArg, objectId);
  }
  
  @Override
  public TaskDto get(RouterObject routerObject) throws NotFoundException {

    routerObject.setRouterId(configuration.getCommsRouterId());

    return getItem(new RouterObjectId(routerObject.getId(), routerObject.getRouterId()));
  }

  @Override
  public List<TaskDto> list(String routerId) {
    return getList(routerId);
  }

  @Override
  public PaginatedList<TaskDto> listPage(String routerId, int page, int perPage) {
    return getList(routerId, page, perPage);
  }

  @Override
  public void delete(RouterObject routerObject) {
    routerObject.setRouterId(configuration.getCommsRouterId());
    super.delete(new RouterObjectId(routerObject.getId(), routerObject.getRouterId()));
  }

}
