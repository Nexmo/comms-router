package com.softavail.api.test;
import com.softavail.api.test.Resource;
import java.util.HashMap;
import java.util.List;
import java.util.Arrays;

import static io.restassured.RestAssured.*;
import io.restassured.RestAssured;
import static io.restassured.matcher.RestAssuredMatchers.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.dto.model.TaskDto;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


public class Task extends Resource{
    private static final Logger LOGGER = LogManager.getLogger(Task.class);
    public Task(HashMap<CommsRouterResource,String> state){
        super(state);
        state.put(CommsRouterResource.TASK,"id");
        RestAssured.baseURI = System.getProperty("autHost");
        RestAssured.basePath= "/comms-router-web/api";
    }
    public List<TaskDto> list(){
        TaskDto[] routers =given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER)).when()
            .get("/routers/{routerId}/tasks")
            .then().statusCode(200)
            .extract().as(TaskDto[].class);
        return Arrays.asList(routers);
    }

    public ApiObject replace(CreateTaskArg args){
        String id = state().get(CommsRouterResource.TASK);
        ApiObject oid = given()
            .contentType("application/json")
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId", id)
            .body(args)
            .when().put("/routers/{routerId}/tasks/{queueId}")
            .then().statusCode(204) // TODO check if 201 is the right code
            .extract()
            .as(ApiObject.class);
        state().put(CommsRouterResource.TASK, oid.getId());
        return oid;
    }

    public ApiObject create(CreateTaskArg args){
        LOGGER.info(given()
                    .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
                    .contentType("application/json")
                    .body(args)
                    .when().post("/routers/{routerId}/tasks").asString());
        ApiObject oid = given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .contentType("application/json")
            .body(args)
            .when().post("/routers/{routerId}/tasks")
            .then().statusCode(201).body("id", not(isEmptyString()) )
            .extract()
            .as(ApiObject.class);
        String id=oid.getId();
        state().put(CommsRouterResource.TASK,id);
        return oid;
    }

    public void delete(){
        String id = state().get(CommsRouterResource.TASK);
        given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId",id)
            .when().delete("/routers/{routerId}/tasks/{queueId}")
            .then().statusCode(204);
    }

    public TaskDto get(){
        String id = state().get(CommsRouterResource.TASK);
        return given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId",id)
            .when().get("/routers/{routerId}/tasks/{queueId}")
            .then().statusCode(200).body("id",equalTo(id))
            .extract().as(TaskDto.class);
    }

    public void update(CreateTaskArg args){
        String id = state().get(CommsRouterResource.TASK);
        given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId", id)
            .body(args)
            .when().post("/routers/{routerId}/tasks/{queueId}")
            .then().statusCode(204);
    }
}
