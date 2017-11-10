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
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.RouterService;

import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class RouterServiceClient extends ServiceClientBase<RouterDto, ApiObjectId>
    implements RouterService {

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
  public ApiObjectId create(CreateRouterArg createArg)
      throws CommsRouterException {

    return post(createArg);
  }

  @Override
  public ApiObjectId create(CreateRouterArg createArg, String routerId)
      throws CommsRouterException {

    return put(createArg, routerId);
  }

  @Override
  public void update(UpdateRouterArg updateArg, String id)
      throws NotFoundException {

    post(updateArg, id);
  }

  @Override
  public RouterDto get(String id)
      throws NotFoundException {

    return getItem(new ApiObjectId(id));
  }

  @Override
  public List<RouterDto> list()
      throws CommsRouterException {

    return getList();
  }

  @Override
  public void delete(String id)
      throws CommsRouterException {

    deleteRequest(new ApiObjectId(id));
  }

}
