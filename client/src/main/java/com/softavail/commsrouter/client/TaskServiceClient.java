package com.softavail.commsrouter.client;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.TaskService;

import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class TaskServiceClient extends ServiceClientBase<TaskDto, CreatedTaskDto>
    implements TaskService {

  private final Client client;
  private final String endpoint;
  private final String routerId;

  @Inject
  public TaskServiceClient(Client client, String endpoint, String routerId) {
    super(TaskDto.class, CreatedTaskDto.class);
    this.client = client;
    this.endpoint = endpoint;
    this.routerId = routerId;
  }

  @Override
  UriBuilder getApiUrl() {
    return UriBuilder.fromPath(endpoint).path("routers")
        .path("{routerId}").path("tasks").clone();
  }

  @Override
  Client getClient() {
    return client;
  }

  @Override
  public CreatedTaskDto create(CreateTaskArg createArg, String routerId)
      throws CommsRouterException {

    // post on container, creates object with auto generated id
    return post(createArg, routerId);
  }

  @Override
  public CreatedTaskDto create(CreateTaskArg createArg, RouterObjectId routerObjectId)
      throws CommsRouterException {

    return put(createArg, routerObjectId);
  }

  @Override
  public void update(UpdateTaskArg updateArg, RouterObjectId routerObjectId)
      throws CommsRouterException {

    // post on resource, updates it with parameters provided
    post(updateArg, routerObjectId);
  }

  @Override
  public void update(UpdateTaskContext taskContext, RouterObjectId routerObjectId)
      throws CommsRouterException {

    post(taskContext, new RouterObjectId(routerObjectId.getId(), routerId));
  }

  @Override
  public TaskDto get(RouterObjectId routerObject)
      throws NotFoundException {

    routerObject.setRouterId(routerId);
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
  public void delete(RouterObjectId routerObject) {
    routerObject.setRouterId(routerId);
    deleteRequest(new RouterObjectId(routerObject.getId(), routerObject.getRouterId()));
  }

}
