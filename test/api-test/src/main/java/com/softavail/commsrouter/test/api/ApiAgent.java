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

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.isEmptyString;
import static org.hamcrest.Matchers.not;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;

import io.restassured.response.ValidatableResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;

public class ApiAgent extends Resource {

  private static final Logger LOGGER = LogManager.getLogger(ApiAgent.class);

  public ApiAgent(HashMap<CommsRouterResource, String> state) {
    super(state);
  }

  public ValidatableResponse list(String routerRef) {
    return list(routerRef,"");
  }
  
  public ValidatableResponse list(String routerRef, String query) {
    return given()
      .pathParam("query", query)
      .contentType("application/json")
      .pathParam("routerRef", routerRef)
      .when().get("/routers/{routerRef}/agents?{query}")
      .then();
  }

  public ValidatableResponse get(String routerRef, String agentRef) {
    return given()
        .contentType("application/json")
        .pathParam("routerRef", routerRef)
        .pathParam("agentRef", agentRef)
        .when().get("/routers/{routerRef}/agents/{agentRef}")
        .then();
  }

  public ValidatableResponse delete(String routerRef, String agentRef) {
    return given()
        .contentType("application/json")
        .pathParam("routerRef", routerRef)
        .pathParam("agentRef", agentRef)
        .when().delete("/routers/{routerRef}/agents/{agentRef}")
        .then();
  }

  public ValidatableResponse create(String routerRef, CreateAgentArg args) {
    return given()
        .contentType("application/json")
        .pathParam("routerRef", routerRef)
        .body(args)
        .when().post("/routers/{routerRef}/agents")
        .then();
  }

  public ValidatableResponse update(String routerRef, String agentRef, UpdateAgentArg args) {
    return given()
        .contentType("application/json")
        .pathParam("routerRef", routerRef)
        .pathParam("agentRef", agentRef)
        .body(args)
        .when().post("/routers/{routerRef}/agents/{agentRef}")
        .then();
  }

  public ValidatableResponse replace(String routerRef, String agentRef, CreateAgentArg args) {
    return given()
        .contentType("application/json")
        .pathParam("routerRef", routerRef)
        .pathParam("agentRef", agentRef)
        .body(args)
        .when().put("/routers/{routerRef}/agents/{agentRef}")
        .then();
  }

}
