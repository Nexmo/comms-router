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
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isEmptyString;
import static org.hamcrest.Matchers.not;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import io.restassured.response.ValidatableResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import javax.ws.rs.core.HttpHeaders;

public class Queue extends GResource<CreateQueueArg, UpdateQueueArg> {

  private static final Logger LOGGER = LogManager.getLogger(Queue.class);

  public Queue(HashMap<CommsRouterResource, String> state) {
    super(state,"/routers/{routerRef}/queues");
  }

  public List<QueueDto> list() {
    QueueDto[] routers = given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .when().get("/routers/{routerId}/queues")
        .then().statusCode(200)
        .extract()
        .as(QueueDto[].class);
    return Arrays.asList(routers);
  }


  public ValidatableResponse replaceResponse(CreateQueueArg args) {
    String ref = state().get(CommsRouterResource.QUEUE);
    ValidatableResponse response =  given()
        .header(HttpHeaders.IF_MATCH, state().get(CommsRouterResource.EQUEUE))
        .contentType("application/json")
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref)
        .body(args)
        .when().put("/routers/{routerRef}/queues/{ref}")
        .then()
        .header(HttpHeaders.ETAG, not(equalTo(null)));
    state().put(CommsRouterResource.EQUEUE, response.extract().header(HttpHeaders.ETAG));
    return response;
  }

  public ApiObjectRef replace(CreateQueueArg args) {
    String id = state().get(CommsRouterResource.QUEUE);
    ApiObjectRef oid = replaceResponse(args)
          .statusCode(201)
          .extract()
          .as(ApiObjectRef.class);
    state().put(CommsRouterResource.QUEUE, oid.getRef());
    return oid;
  }

  public ApiObjectRef create(CreateQueueArg args) {
    ValidatableResponse response = given()
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .contentType("application/json")
        .body(args)
        .when().post("/routers/{routerRef}/queues")
        .then().statusCode(201)
        .body("id", not(isEmptyString()))
        .header(HttpHeaders.ETAG, not(equalTo(null)));
    
    ApiObjectRef oid = response.extract()
        .as(ApiObjectRef.class);
    String id = oid.getRef();
    state().put(CommsRouterResource.QUEUE, id);
    state().put(CommsRouterResource.EQUEUE, response.extract().header(HttpHeaders.ETAG));
    return oid;
  }

  public void delete(String ref) {
    given()
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref)
        .when().delete("/routers/{routerRef}/queues/{ref}")
        .then().statusCode(204);
  }

  public void delete() {
    String ref = state().get(CommsRouterResource.QUEUE);
    delete(ref);
  }

  public QueueDto get() {
    String ref = state().get(CommsRouterResource.QUEUE);
    return given()
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref)
        .when().get("/routers/{routerRef}/queues/{ref}")
        .then().statusCode(200)
        .header(HttpHeaders.ETAG, not(equalTo(null)))
        .body("ref", equalTo(ref)).extract().as(QueueDto.class);
  }

  public Integer size() {
    String ref = state().get(CommsRouterResource.QUEUE);
    return given().pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref).when().get("/routers/{routerRef}/queues/{ref}/size").then()
        .statusCode(200).extract().path("size");
  }

  public void checkSize(Integer size) {
    String ref = state().get(CommsRouterResource.QUEUE);
    given().pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
      .pathParam("ref", ref).when().get("/routers/{routerRef}/queues/{ref}/size").then()
      .statusCode(200).body("size",is(size));
  }

  public List<TaskDto> tasks() {
    String ref = state().get(CommsRouterResource.QUEUE);
    TaskDto[] qtasks = given()
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref)
        .when().get("/routers/{routerRef}/queues/{ref}/tasks")
        .then().statusCode(200)
        .extract()
        .as(TaskDto[].class);
    return Arrays.asList(qtasks);
  }

  public void update(CreateQueueArg args) {
    String ref = state().get(CommsRouterResource.QUEUE);
    ValidatableResponse response = given()
        .header(HttpHeaders.IF_MATCH, state().get(CommsRouterResource.EQUEUE))
        .contentType("application/json")
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref)
        .body(args)
        .when().post("/routers/{routerRef}/queues/{ref}")
        .then().statusCode(204);
    state().put(CommsRouterResource.EQUEUE, response.extract().header(HttpHeaders.ETAG));
  }
}
