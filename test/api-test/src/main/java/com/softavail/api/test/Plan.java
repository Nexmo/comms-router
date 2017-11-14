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

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;


public class Plan extends Resource {

  private static final Logger LOGGER = LogManager.getLogger(Plan.class);

  public Plan(HashMap<CommsRouterResource, String> state) {
    super(state);
  }

  public List<PlanDto> list() {
    PlanDto[] routers = given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER)).when()
        .get("/routers/{routerId}/plans")
        .then().statusCode(200)
        .extract().as(PlanDto[].class);
    return Arrays.asList(routers);
  }

  public ApiObjectId replace(CreatePlanArg args) {
    String id = state().get(CommsRouterResource.PLAN);
    ApiObjectId oid = given()
        .contentType("application/json")
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .body(args)
        .when().put("/routers/{routerId}/plans/{queueId}")
        .then().statusCode(201)
        .extract()
        .as(ApiObjectId.class);
    state().put(CommsRouterResource.PLAN, oid.getId());
    return oid;
  }

  public ApiObjectId create(CreatePlanArg args) {
    ApiObjectId oid = given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .contentType("application/json")
        .body(args)
        .when().post("/routers/{routerId}/plans")
        .then().statusCode(201).body("id", not(isEmptyString()))
        .extract()
        .as(ApiObjectId.class);
    String id = oid.getId();
    state().put(CommsRouterResource.PLAN, id);
    return oid;
  }

  public void delete() {
    String id = state().get(CommsRouterResource.PLAN);
    given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .when().delete("/routers/{routerId}/plans/{queueId}")
        .then().statusCode(204);
  }

  public PlanDto get() {
    String id = state().get(CommsRouterResource.PLAN);
    return given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .when().get("/routers/{routerId}/plans/{queueId}")
        .then().statusCode(200).body("id", equalTo(id))
        .extract().as(PlanDto.class);
  }

  public void update(CreatePlanArg args) {
    String id = state().get(CommsRouterResource.PLAN);
    given()
        .contentType("application/json")
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .body(args)
        .when().post("/routers/{routerId}/plans/{queueId}")
        .then().statusCode(204);
  }

}
