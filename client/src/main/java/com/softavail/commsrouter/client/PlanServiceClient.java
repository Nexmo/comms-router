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

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.PlanService;

import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class PlanServiceClient extends ServiceClientBase<PlanDto, ApiObjectRef>
    implements PlanService {

  private final Client client;
  private final String endpoint;
  private final String routerRef;

  @Inject
  public PlanServiceClient(Client client, String endpoint, String routerRef) {
    this.client = client;
    this.endpoint = endpoint;
    this.routerRef = routerRef;
  }

  @Override
  UriBuilder getApiUrl() {
    return UriBuilder.fromPath(endpoint)
        .path("routers")
        .path("{routerId}")
        .path("plans")
        .clone();
  }

  @Override
  Client getClient() {
    return client;
  }

  @Override
  public PlanDto get(RouterObjectRef routerObject)
      throws NotFoundException {

    return getItem(new RouterObjectRef(routerObject.getRef(), routerObject.getRouterRef()));
  }

  @Override
  public PaginatedList<PlanDto> list(PagingRequest request) {
    PagingRequest pagingRequest = new PagingRequest(
        routerRef, request.getToken(), request.getPerPage(), request.getSort(), request.getQuery());
    return getList(pagingRequest, new GenericType<List<PlanDto>>() {});
  }

  @Override
  public void delete(RouterObjectRef routerObject) {
    routerObject.setRouterRef(routerRef);
    deleteRequest(new RouterObjectRef(routerObject.getRef(), routerObject.getRouterRef()));
  }

  @Override
  public ApiObjectRef create(CreatePlanArg createArg, String ref) {

    return post(createArg, ref);
  }

  @Override
  public ApiObjectRef replace(CreatePlanArg createArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    return put(createArg, objectRef);
  }

  @Override
  public void update(UpdatePlanArg updateArg, RouterObjectRef ref)
      throws NotFoundException {

    post(updateArg, ref);
  }

}
