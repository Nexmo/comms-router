/* 
 * Copyright 2017 SoftAvail Inc.
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

package com.softavail.commsrouter.client;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.TaskService;

import java.net.URI;
import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class TaskServiceClient extends ServiceClientBase<TaskDto, CreatedTaskDto>
    implements TaskService {

  private final Client client;
  private final String endpoint;
  private final String routerRef;

  @Inject
  public TaskServiceClient(Client client, String endpoint, String routerRef) {
    this.client = client;
    this.endpoint = endpoint;
    this.routerRef = routerRef;
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
  public CreatedTaskDto replace(CreateTaskArg createArg, RouterObjectRef routerObjectId)
      throws CommsRouterException {

    return put(createArg, routerObjectId);
  }

  @Override
  public void update(UpdateTaskArg updateArg, RouterObjectRef routerObjectId)
      throws CommsRouterException {

    // post on resource, updates it with parameters provided
    post(updateArg, routerObjectId);
  }

  @Override
  public void update(UpdateTaskContext taskContext, RouterObjectRef routerObjectId)
      throws CommsRouterException {

    putContext(taskContext, new RouterObjectRef(routerObjectId.getRef(), routerRef));
  }

  @Override
  public void updateContext(UpdateTaskContext taskContext, RouterObjectRef routerObjectId)
      throws CommsRouterException {

    postContext(taskContext, new RouterObjectRef(routerObjectId.getRef(), routerRef));
  }

  @Override
  public TaskDto get(RouterObjectRef routerObject)
      throws NotFoundException {

    routerObject.setRouterRef(routerRef);
    return getItem(new RouterObjectRef(routerObject.getRef(), routerObject.getRouterRef()));
  }

  @Override
  public TaskDto getByTag(String routerId, String tag)
      throws CommsRouterException {

    URI uri = getApiUrl().clone()
        .path("byTag")
        .queryParam("tag", tag)
        .build(routerId);

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .get(TaskDto.class);
  }

  @Override
  public PaginatedList<TaskDto> list(PagingRequest request) {
    PagingRequest pagingRequest = new PagingRequest(
        routerRef, request.getToken(), request.getPerPage(), request.getSort(), request.getQuery());
    return getList(pagingRequest, new GenericType<List<TaskDto>>() {});
  }

  @Override
  public void delete(RouterObjectRef routerObject) {
    routerObject.setRouterRef(routerRef);
    deleteRequest(new RouterObjectRef(routerObject.getRef(), routerObject.getRouterRef()));
  }

  // POST over resource updates. Returns void
  private void postContext(Object obj, RouterObjectRef id) {
    URI uri = getApiUrl().clone()
        .path("{resourceId}")
        .path("user_context")
        .build(id.getRouterRef(), id.getRef());

    getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .post(Entity.entity(obj, MediaType.APPLICATION_JSON_TYPE));
  }

  protected CreatedTaskDto putContext(Object obj, RouterObjectRef id) {
    URI uri = getApiUrl().clone()
        .path("{resourceId}")
        .path("user_context")
        .build(id.getRouterRef(), id.getRef());

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .put(Entity.entity(obj, MediaType.APPLICATION_JSON_TYPE), CreatedTaskDto.class);
  }

}
