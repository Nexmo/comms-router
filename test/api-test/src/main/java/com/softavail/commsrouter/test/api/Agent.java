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
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;

import io.restassured.response.ValidatableResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import javax.ws.rs.core.HttpHeaders;

public class Agent extends GResource<CreateAgentArg, UpdateAgentArg> {

  private static final Logger LOGGER = LogManager.getLogger(Agent.class);

  public Agent(HashMap<CommsRouterResource, String> state) {
    super(state,"/routers/{routerRef}/agents");
  }

  public List<AgentDto> list() {
    AgentDto[] routers = given()
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .when().get("/routers/{routerRef}/agents")
        .then().statusCode(200)
        .extract()
        .as(AgentDto[].class);
    return Arrays.asList(routers);
  }

  public ApiObjectRef replace(CreateAgentArg args) {
    String ref = state().get(CommsRouterResource.AGENT);
    ValidatableResponse response = given()
        .contentType("application/json")
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref)
        .body(args)
        .when().put("/routers/{routerRef}/agents/{ref}")
        .then();
    
    ApiObjectRef oid = response.statusCode(201)
        .extract()
        .as(ApiObjectRef.class);
    state().put(CommsRouterResource.AGENT, oid.getRef());
    state().put(CommsRouterResource.EAGENT, response.extract().header(HttpHeaders.ETAG));
    return oid;
  }

  public ApiObjectRef create(CreateAgentArg args) {
    ValidatableResponse response = given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .contentType("application/json")
        .body(args)
        .when().post("/routers/{routerId}/agents")
        .then().statusCode(201)
        .header(HttpHeaders.ETAG, not(equalTo(null)))
        .body("ref", not(isEmptyString()));
    ApiObjectRef oid = response.extract()
        .as(ApiObjectRef.class);
    
    state().put(CommsRouterResource.AGENT, oid.getRef());
    state().put(CommsRouterResource.EAGENT, response.extract().header(HttpHeaders.ETAG));
    return oid;
  }

  public ApiObjectRef create(String language) {
    CreateAgentArg arg = new CreateAgentArg();
    arg.setAddress("phonenumber");
    arg.setCapabilities(
        new AttributeGroupDto().withKeyValue("language", new StringAttributeValueDto(language)));
    return create(arg);
  }

  public void delete(String ref) {
    given()
      .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
      .pathParam("ref", ref)
      .when().delete("/routers/{routerRef}/agents/{ref}")
      .then().statusCode(204);
  }

  public void delete() {
    delete(state().get(CommsRouterResource.AGENT));
  }

  public AgentDto get(String ref) {
    ValidatableResponse response = given()
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref).when().get("/routers/{routerRef}/agents/{ref}").then();
    state().put(CommsRouterResource.EAGENT, response.extract().header(HttpHeaders.ETAG));      
    return response
        .statusCode(200).body("ref", equalTo(ref))
        .extract()
        .as(AgentDto.class);
  }

  public AgentDto get() {
    return get(state().get(CommsRouterResource.AGENT));
  }

  public void update(UpdateAgentArg args) {
    String ref = state().get(CommsRouterResource.AGENT);
    ValidatableResponse response = given()
        .header(HttpHeaders.IF_MATCH, state().get(CommsRouterResource.EAGENT))
        .contentType("application/json")
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref)
        .body(args)
        .when().post("/routers/{routerId}/agents/{ref}")
        .then()
        .header(HttpHeaders.ETAG, not(equalTo(null)))
        .statusCode(204);
    state().put(CommsRouterResource.EAGENT, response.extract().header(HttpHeaders.ETAG));
  }

  public void setState(AgentState state) {
    UpdateAgentArg agentArg = new UpdateAgentArg();
    agentArg.setState(state);
    update(agentArg);
  }
}
