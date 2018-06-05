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

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.dto.misc.SizeDto;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.QueueService;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 05.09.17.
 */
public class QueueServiceClient extends ServiceClientBase<QueueDto, ApiObjectRef>
    implements QueueService {

  private final Client client;
  private final String endpoint;
  private final String routerRef;

  @Inject
  public QueueServiceClient(Client client, String endpoint, String routerId) {
    this.client = client;
    this.endpoint = endpoint;
    this.routerRef = routerId;
  }

  @Override
  UriBuilder getApiUrl() {
    return UriBuilder.fromPath(endpoint).path("routers")
        .path("{routerId}").path("queues").clone();
  }

  @Override
  Client getClient() {
    return client;
  }

  @Override
  public QueueDto get(RouterObjectRef routerObject)
      throws NotFoundException {

    return getItem(new RouterObjectRef(routerObject.getRef(), routerObject.getRouterRef()));
  }

  @Override
  public PaginatedList<QueueDto> list(PagingRequest request) {
    PagingRequest pagingRequest = new PagingRequest(
        routerRef, request.getToken(), request.getPerPage(), request.getSort(), request.getQuery());
    return getList(pagingRequest, new GenericType<List<QueueDto>>() {});
  }

  @Override
  public void delete(RouterObjectRef routerObject) {
    routerObject.setRouterRef(routerRef);
    super.deleteRequest(new RouterObjectRef(routerObject.getRef(), routerObject.getRouterRef()));
  }

  @Override
  public ApiObjectRef create(CreateQueueArg createArg, String ref)
      throws NotFoundException {

    return post(createArg, ref);
  }

  @Override
  public ApiObjectRef replace(CreateQueueArg createArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    return put(createArg, objectRef);
  }

  @Override
  public void update(UpdateQueueArg updateArg, RouterObjectRef ref)
      throws NotFoundException {

    post(updateArg, ref);
  }

  @Override
  public long getQueueSize(RouterObjectRef routerObjectRef)
      throws NotFoundException {

    URI uri = getApiUrl().clone()
        .path("{resourceRef}")
        .path("size")
        .build(routerObjectRef.getRouterRef(), routerObjectRef.getRef());

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .get(SizeDto.class)
        .getSize();
  }

  @Override
  public Collection<TaskDto> getTasks(RouterObjectRef routerObjectId)
      throws NotFoundException {

    URI uri = getApiUrl().clone()
        .path("{resourceRef}")
        .path("tasks")
        .build(routerObjectId.getRouterRef(), routerObjectId.getRef());

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .get(new GenericType<Collection<TaskDto>>(){});
  }

}
