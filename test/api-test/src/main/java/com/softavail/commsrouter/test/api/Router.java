/*
 * Copyright 2017 SoftAvail, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.softavail.commsrouter.test.api;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.isEmptyString;
import static org.hamcrest.Matchers.not;

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import io.restassured.response.ValidatableResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import javax.ws.rs.core.HttpHeaders;

public class Router extends GResource<CreateRouterArg, UpdateRouterArg> {

  private static final Logger LOGGER = LogManager.getLogger(Router.class);

  public Router(HashMap<CommsRouterResource, String> state) {
    super(state,"/routers");
  }

  public List<RouterDto> list(String query) {
    RouterDto[] routers = list(querySpec(query))
            .statusCode(200)
            .extract()
            .as(RouterDto[].class);
    return Arrays.asList(routers);
  }

  public List<RouterDto> list() {
    return list("");
  }

  public ValidatableResponse replaceResponse(CreateRouterArg args) {
    String routerRef = state().get(CommsRouterResource.ROUTER);
    return replace(getSpec(state().get(CommsRouterResource.ROUTER)),args)
        .header(HttpHeaders.ETAG, not(equalTo(null)));
  }

  public ApiObjectRef replace(CreateRouterArg args) {
    ValidatableResponse response = replaceResponse(args).statusCode(201);
    ApiObjectRef oid = response.extract()
        .as(ApiObjectRef.class);
    state().put(CommsRouterResource.ROUTER, oid.getRef());
    state().put(CommsRouterResource.EROUTER, response.extract().header(HttpHeaders.ETAG));
    return oid;
  }

  public ApiObjectRef create(CreateRouterArg args) {
    ValidatableResponse response = create(createSpec(),args)
        .statusCode(201)
        .header(HttpHeaders.ETAG, not(equalTo(null)))
        .body("ref", not(isEmptyString()));
    
    ApiObjectRef oid = response.extract()
        .as(ApiObjectRef.class);
    String id = oid.getRef();
    
    state().put(CommsRouterResource.ROUTER, id);
    state().put(CommsRouterResource.EROUTER, response.extract().header(HttpHeaders.ETAG));
    return oid;
  }

  public ValidatableResponse deleteResponse() {
    return delete(getSpec(state().get(CommsRouterResource.ROUTER)));
  }

  public void delete() {
    deleteResponse().statusCode(204);
  }

  public RouterDto get() {
    String routerRef = state().get(CommsRouterResource.ROUTER);
    return get(getSpec(routerRef))
        .statusCode(200)
        .body("ref", equalTo(routerRef))
        .extract()
        .as(RouterDto.class);
  }

  public void update(UpdateRouterArg args) {

    ValidatableResponse response = update(getSpecEtag(state().get(CommsRouterResource.EROUTER),
                                                  state().get(CommsRouterResource.ROUTER)),
                                          args)
        .header(HttpHeaders.ETAG, not(equalTo(null)))
        .statusCode(204);
    state().put(CommsRouterResource.EROUTER, response.extract().header(HttpHeaders.ETAG));
  }

}
