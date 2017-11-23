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
import static org.hamcrest.Matchers.isA;
import static org.hamcrest.Matchers.isEmptyString;
import static org.hamcrest.Matchers.not;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class Task extends Resource {

  private static final Logger LOGGER = LogManager.getLogger(Task.class);

  public Task(HashMap<CommsRouterResource, String> state) {
    super(state);
  }

  public List<TaskDto> list() {
    TaskDto[] routers = given().pathParam("routerRef", state().get(CommsRouterResource.ROUTER)).when()
            .get("/routers/{routerRef}/tasks").then().statusCode(200).extract().as(TaskDto[].class);
    return Arrays.asList(routers);
  }

  public CreatedTaskDto replace(CreateTaskArg args) {
    String ref = state().get(CommsRouterResource.TASK);
    CreatedTaskDto oid = given().contentType("application/json")
        .pathParam("routerRef", state().get(CommsRouterResource.ROUTER)).pathParam("ref", ref)
        .body(args).when().put("/routers/{routerRef}/tasks/{ref}").then().statusCode(201).extract()
        .as(CreatedTaskDto.class);
    state().put(CommsRouterResource.TASK, oid.getRef());
    return oid;
  }

  public CreatedTaskDto create(CreateTaskArg args) {
    CreatedTaskDto oid = given().pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .contentType("application/json")
        .body(args).when().post("/routers/{routerRef}/tasks").then().statusCode(201)
        .body("ref", not(isEmptyString())).and().body("queueTasks", isA(Integer.class)).extract()
        .as(CreatedTaskDto.class);
    String id = oid.getRef();
    state().put(CommsRouterResource.TASK, id);
    return oid;
  }

  public CreatedTaskDto createQueueTask(URL url) {
    CreateTaskArg taskArg = new CreateTaskArg();
    taskArg.setCallbackUrl(url);
    taskArg.setRequirements(new AttributeGroupDto());
    taskArg.setQueueRef(state().get(CommsRouterResource.QUEUE));
    return create(taskArg);
  }

  public CreatedTaskDto createQueueTask() throws MalformedURLException {
    return createQueueTask(new URL("http://example.com"));
  }

  public CreatedTaskDto createWithPlan(CreateTaskArg args) {
    args.setPlanRef(state().get(CommsRouterResource.PLAN));
    CreatedTaskDto oid = given().pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .contentType("application/json")
        .body(args).when().post("/routers/{routerRef}/tasks").then().statusCode(201)
        .body("ref", not(isEmptyString())).extract().as(CreatedTaskDto.class);
    String id = oid.getRef();
    state().put(CommsRouterResource.TASK, id);
    return oid;
  }

  public void delete() {
    String ref = state().get(CommsRouterResource.TASK);
    given().pathParam("routerRef", state().get(CommsRouterResource.ROUTER)).pathParam("ref", ref)
        .when().delete("/routers/{routerRef}/tasks/{ref}").then().statusCode(204);
  }

  public TaskDto get() {
    String ref = state().get(CommsRouterResource.TASK);
    return given().pathParam("routerRef", state().get(CommsRouterResource.ROUTER))
        .pathParam("ref", ref).when().get("/routers/{routerRef}/tasks/{ref}").then().statusCode(200)
        .body("ref", equalTo(ref)).extract().as(TaskDto.class);
  }

  public void update(UpdateTaskArg args) {
    String ref = state().get(CommsRouterResource.TASK);
    given().pathParam("routerRef", state().get(CommsRouterResource.ROUTER)).pathParam("ref", ref)
        .contentType("application/json").body(args).when().post("/routers/{routerRef}/tasks/{ref}")
        .then().statusCode(204);
  }

  public void setState(TaskState state) {
    UpdateTaskArg arg = new UpdateTaskArg();
    arg.setState(state);
    update(arg);
  }

}
