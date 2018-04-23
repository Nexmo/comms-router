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


import com.softavail.commsrouter.api.dto.arg.CreateSkillArg;
import com.softavail.commsrouter.api.dto.arg.UpdateSkillArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;

import com.softavail.commsrouter.api.dto.model.skill.SkillDto;

import io.restassured.response.ValidatableResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import javax.ws.rs.core.HttpHeaders;

public class Skill extends GResource<CreateSkillArg, UpdateSkillArg> {

  private static final Logger LOGGER = LogManager.getLogger(Skill.class);

  public Skill(HashMap<CommsRouterResource, String> state) {
    super(state,"/routers/{routerRef}/skills");
  }

  public List<SkillDto> list() {
    SkillDto[] routers = given()
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .when().get("/routers/{routerRef}/skills")
        .then().statusCode(200)
        .extract()
        .as(SkillDto[].class);
    return Arrays.asList(routers);
  }

  public ApiObjectRef replace(CreateSkillArg args) {
    String ref = state().get(CommsRouterResource.SKILL);
    return replace(ref, args);
  }

  public ApiObjectRef replace(String ref, CreateSkillArg args) {
    ApiObjectRef oid = given()
        .contentType("application/json")
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref)
        .body(args)
        .when().put("/routers/{routerRef}/skills/{ref}")
        .then().statusCode(201)
        .extract()
        .as(ApiObjectRef.class);
    state().put(CommsRouterResource.SKILL, ref);
    return oid;
  }

  public ApiObjectRef create(CreateSkillArg args) {
    ValidatableResponse response = given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .contentType("application/json")
        .body(args)
        .when().post("/routers/{routerId}/skills")
        .then();
    
    ApiObjectRef oid = response
        .header(HttpHeaders.ETAG, not(equalTo(null)))
        .statusCode(201)
        .body("ref", not(isEmptyString()))
        .extract()
        .as(ApiObjectRef.class);
    String id = oid.getRef();
    state().put(CommsRouterResource.SKILL, id);
    state().put(CommsRouterResource.ESKILL, response.extract().header(HttpHeaders.ETAG));
    return oid;
  }

  public void delete(String ref) {
    given()
      .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
      .pathParam("ref", ref)
      .when().delete("/routers/{routerRef}/skills/{ref}")
      .then().statusCode(204);
  }

  public void delete() {
    String ref = state().get(CommsRouterResource.SKILL);
    delete(ref);
  }

  public SkillDto get(String ref) {
    return given()
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref).when().get("/routers/{routerRef}/skills/{ref}").then()
        .statusCode(200).body("ref", equalTo(ref))
        .extract()
        .as(SkillDto.class);
  }

  public SkillDto get() {
    return get(state().get(CommsRouterResource.SKILL));
  }

  public void update(UpdateSkillArg args) {
    String ref = state().get(CommsRouterResource.SKILL);
    ValidatableResponse response = given()
        .header(HttpHeaders.IF_MATCH, state().get(CommsRouterResource.ESKILL))
        .contentType("application/json")
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref)
        .body(args)
        .when().post("/routers/{routerId}/skills/{ref}")
        .then()
        .header(HttpHeaders.ETAG, not(equalTo(null)))
        .statusCode(204);
    state().put(CommsRouterResource.ESKILL, response.extract().header(HttpHeaders.ETAG));
  }

}
