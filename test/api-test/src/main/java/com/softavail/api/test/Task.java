package com.softavail.api.test;

import com.softavail.api.test.Resource;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import java.util.HashMap;
import java.util.List;
import java.util.Arrays;
import java.net.URL;
import java.net.MalformedURLException;

import static io.restassured.RestAssured.*;

import io.restassured.RestAssured;

import static io.restassured.matcher.RestAssuredMatchers.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;

import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;



public class Task extends Resource {

  private static final Logger LOGGER = LogManager.getLogger(Task.class);

  public Task(HashMap<CommsRouterResource, String> state) {
    super(state);
  }

  public List<TaskDto> list() {
    TaskDto[] routers = given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER)).when()
        .get("/routers/{routerId}/tasks")
        .then().statusCode(200)
        .extract().as(TaskDto[].class);
    return Arrays.asList(routers);
  }

  public CreatedTaskDto replace(CreateTaskArg args) {
    String id = state().get(CommsRouterResource.QUEUE);
    CreatedTaskDto oid = given()
        .contentType("application/json")
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .body(args)
        .when().put("/routers/{routerId}/tasks/{queueId}")
        .then().statusCode(201)
        .extract()
        .as(CreatedTaskDto.class);
    state().put(CommsRouterResource.TASK, oid.getId());
    return oid;
  }

  public CreatedTaskDto create(CreateTaskArg args) {
    CreatedTaskDto oid = given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .contentType("application/json")
        .body(args)
        .when().post("/routers/{routerId}/tasks").then().statusCode(201)
        .body("id", not(isEmptyString())).and().body("queueTasks", isA(Integer.class))
        .extract()
        .as(CreatedTaskDto.class);
    String id = oid.getId();
    state().put(CommsRouterResource.TASK, id);
    return oid;
  }

  public CreatedTaskDto createQueueTask(URL url) {
    CreateTaskArg taskArg = new CreateTaskArg();
    taskArg.setCallbackUrl(url);
    taskArg.setRequirements(new AttributeGroupDto());
    taskArg.setQueueId(state().get(CommsRouterResource.QUEUE));
    return create(taskArg);
  }

  public CreatedTaskDto createQueueTask()  throws MalformedURLException{
    return createQueueTask(new URL("http://example.com"));
  }

  public CreatedTaskDto createWithPlan(CreateTaskArg args) {
    args.setPlanId(state().get(CommsRouterResource.PLAN));
    CreatedTaskDto oid = given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .contentType("application/json")
        .body(args)
        .when().post("/routers/{routerId}/tasks")
        .then().statusCode(201).body("id", not(isEmptyString()))
        .extract()
        .as(CreatedTaskDto.class);
    String id = oid.getId();
    state().put(CommsRouterResource.TASK, id);
    return oid;
  }

  public void delete() {
    String id = state().get(CommsRouterResource.TASK);
    given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .when().delete("/routers/{routerId}/tasks/{queueId}")
        .then().statusCode(204);
  }

  public TaskDto get() {
    String id = state().get(CommsRouterResource.TASK);
    return given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .when().get("/routers/{routerId}/tasks/{queueId}")
        .then().statusCode(200).body("id", equalTo(id))
        .extract().as(TaskDto.class);
  }

  public void update(UpdateTaskArg args) {
    String id = state().get(CommsRouterResource.TASK);
    given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .contentType("application/json")
        .body(args)
        .when().post("/routers/{routerId}/tasks/{queueId}")
        .then().statusCode(204);
  }

  public void setState(TaskState state) {
    UpdateTaskArg arg = new UpdateTaskArg();
    arg.setState(state);
    update(arg);
  }

}
