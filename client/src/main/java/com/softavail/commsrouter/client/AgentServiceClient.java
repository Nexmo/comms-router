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
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.AgentService;

import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class AgentServiceClient extends ServiceClientBase<AgentDto, ApiObjectId>
    implements AgentService {

  private final Client client;
  private final String endpoint;
  private final String routerId;

  @Inject
  public AgentServiceClient(Client client, String endpoint, String routerId) {
    this.client = client;
    this.endpoint = endpoint;
    this.routerId = routerId;
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
  public ApiObjectId create(CreateAgentArg createArg, String routerId)
      throws CommsRouterException {

    return post(createArg, routerId);
  }

  @Override
  public ApiObjectId create(CreateAgentArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    return put(createArg, objectId);
  }

  @Override
  public void update(UpdateAgentArg updateArg, RouterObjectId objectId)
      throws CommsRouterException {

    put(updateArg, objectId);
  }

  @Override
  public AgentDto get(RouterObjectId routerObjectId)
      throws CommsRouterException {

    return getItem(routerObjectId);
  }

  @Override
  public List<AgentDto> list(String routerId)
      throws CommsRouterException {

    return getList(routerId);
  }

  @Override
  public PaginatedList<AgentDto> list(String routerId, int page, int perPage)
      throws CommsRouterException {

    return getList(routerId, page, perPage);
  }

  @Override
  public void delete(RouterObjectId routerObjectId) {
    routerObjectId.setRouterId(routerId);
    deleteRequest(routerObjectId);
  }

}
