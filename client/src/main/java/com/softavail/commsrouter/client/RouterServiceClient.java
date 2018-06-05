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

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.PaginatedService;
import com.softavail.commsrouter.api.interfaces.RouterService;

import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class RouterServiceClient extends ServiceClientBase<RouterDto, ApiObjectRef>
    implements RouterService, PaginatedService<RouterDto> {

  private Client client;
  private final String endpoint;

  @Inject
  public RouterServiceClient(Client client, String endpoint) {
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
  public ApiObjectRef create(CreateRouterArg createArg)
      throws CommsRouterException {

    return post(createArg);
  }

  @Override
  public ApiObjectRef replace(CreateRouterArg createArg, String routerRef)
      throws CommsRouterException {

    return put(createArg, routerRef);
  }

  @Override
  public void update(UpdateRouterArg updateArg, ApiObjectRef ref)
      throws NotFoundException {

    post(updateArg, ref);
  }

  @Override
  public RouterDto get(String ref)
      throws NotFoundException {

    return getItem(new ApiObjectRef(ref));
  }

  @Override
  public PaginatedList<RouterDto> list(PagingRequest request) {
    return getList(request, new GenericType<List<RouterDto>>() {});
  }

  @Override
  public void delete(String ref)
      throws CommsRouterException {

    deleteRequest(new ApiObjectRef(ref));
  }

}
