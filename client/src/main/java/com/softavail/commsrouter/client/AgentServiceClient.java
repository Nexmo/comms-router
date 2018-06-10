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

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.AgentService;

import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class AgentServiceClient extends ServiceClientBase<AgentDto, ApiObjectRef>
    implements AgentService {

  private final Client client;
  private final String endpoint;
  private final String routerRef;

  @Inject
  public AgentServiceClient(Client client, String endpoint, String routerRef) {
    this.client = client;
    this.endpoint = endpoint;
    this.routerRef = routerRef;
  }

  @Override
  UriBuilder getApiUrl() {
    return UriBuilder.fromPath(endpoint)
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
  public ApiObjectRef create(CreateAgentArg createArg, String routerRef)
      throws CommsRouterException {

    return post(createArg, routerRef);
  }

  @Override
  public ApiObjectRef replace(CreateAgentArg createArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    return put(createArg, objectRef);
  }

  @Override
  public void update(UpdateAgentArg updateArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    post(updateArg, objectRef);
  }

  @Override
  public AgentDto get(RouterObjectRef routerObjectRef)
      throws CommsRouterException {

    return getItem(routerObjectRef);
  }

  @Override
  public PaginatedList<AgentDto> list(PagingRequest request)
      throws CommsRouterException {

    PagingRequest pagingRequest = new PagingRequest(
        routerRef, request.getToken(), request.getPerPage(), request.getSort(), request.getQuery());
    return getList(pagingRequest, new GenericType<List<AgentDto>>() {});
  }

  @Override
  public void delete(RouterObjectRef routerObjectRef) {
    routerObjectRef.setRouterRef(routerRef);
    deleteRequest(routerObjectRef);
  }

}
