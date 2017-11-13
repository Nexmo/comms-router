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

package com.softavail.api.test;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.isEmptyString;
import static org.hamcrest.Matchers.not;

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class Router extends Resource {

  private static final Logger LOGGER = LogManager.getLogger(Router.class);

  public Router(HashMap<CommsRouterResource, String> state) {
    super(state);
  }

  public List<RouterDto> list() {
    RouterDto[] routers = given().when()
        .get("/routers")
        .then().statusCode(200)
        .extract().as(RouterDto[].class);
    return Arrays.asList(routers);
  }

  public ApiObjectId replace(CreateRouterArg args) {
    String id = state().get(CommsRouterResource.ROUTER);
    ApiObjectId oid = given()
        .contentType("application/json")
        .pathParam("routerId", id)
        .body(args)
        .when().put("/routers/{routerId}")
        .then().statusCode(201)
        .extract()
        .as(ApiObjectId.class);
    state().put(CommsRouterResource.ROUTER, oid.getId());
    return oid;
  }

  public ApiObjectId create(CreateRouterArg args) {
    ApiObjectId oid = given()
        .contentType("application/json")
        .body(args)
        .when().post("/routers")
        .then().statusCode(201).body("id", not(isEmptyString()))
        .extract()
        .as(ApiObjectId.class);
    String id = oid.getId();
    state().put(CommsRouterResource.ROUTER, id);
    return oid;
  }

  public void delete() {
    String id = state().get(CommsRouterResource.ROUTER);
    given()
        .pathParam("routerId", id)
        .when().delete("/routers/{routerId}")
        .then().statusCode(204);
  }

  public RouterDto get() {
    String id = state().get(CommsRouterResource.ROUTER);
    return given()
        .pathParam("routerId", id)
        .when().get("/routers/{routerId}")
        .then().statusCode(200).body("id", equalTo(id))
        .extract().as(RouterDto.class);
  }

  public void update(CreateRouterArg args) {
    String id = state().get(CommsRouterResource.ROUTER);
    given()
        .contentType("application/json")
        .pathParam("routerId", id)
        .body(args)
        .when().post("/routers/{routerId}")
        .then().statusCode(204);
  }

}
