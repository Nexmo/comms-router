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

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Queue extends Resource{
    private static final Logger LOGGER = LogManager.getLogger(Queue.class);
    public Queue(HashMap<CommsRouterResource,String> state){
        super(state);
        state.put(CommsRouterResource.QUEUE,"id");
        RestAssured.baseURI = System.getProperty("autHost");
        RestAssured.basePath= "/comms-router-web/api";
    }
    public List<QueueDto> list(){
        QueueDto[] routers =given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER)).when()
            .get("/routers/{routerId}/queues")
            .then().statusCode(200)
            .extract().as(QueueDto[].class);
        return Arrays.asList(routers);
    }

    public ApiObjectId replace(CreateQueueArg args){
        String id = state().get(CommsRouterResource.QUEUE);
        ApiObjectId oid = given()
            .contentType("application/json")
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId", id)
            .body(args)
            .when().put("/routers/{routerId}/queues/{queueId}")
            .then().statusCode(201)
            .extract()
            .as(ApiObjectId.class);
        state().put(CommsRouterResource.QUEUE, oid.getId());
        return oid;
    }

    public ApiObjectId create(CreateQueueArg args){

        ApiObjectId oid = given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .contentType("application/json")
            .body(args)
            .when().post("/routers/{routerId}/queues")
            .then()
            .statusCode(201)
            .body("id", not(isEmptyString()) )
            .extract()
            .as(ApiObjectId.class);
        String id=oid.getId();
        state().put(CommsRouterResource.QUEUE,id);
        return oid;
    }

    public void delete(){
        String id = state().get(CommsRouterResource.QUEUE);
        given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId",id)
            .when().delete("/routers/{routerId}/queues/{queueId}")
            .then().statusCode(204);
    }

    public QueueDto get(){
        String id = state().get(CommsRouterResource.QUEUE);
        return given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId",id)
            .when().get("/routers/{routerId}/queues/{queueId}")
            .then().statusCode(200).body("id",equalTo(id))
            .extract().as(QueueDto.class);
    }

    public Integer size(){
        String id = state().get(CommsRouterResource.QUEUE);
        return given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId",id)
            .when().get("/routers/{routerId}/queues/{queueId}/size")
            .then().log().ifError().statusCode(200)
            .extract().path("size");
    }
    public List<TaskDto> tasks(){
        String id = state().get(CommsRouterResource.QUEUE);
        TaskDto[] qtasks= given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId",id)
            .when().get("/routers/{routerId}/queues/{queueId}/tasks")
            .then().log().ifError().statusCode(200)
            .extract().as(TaskDto[].class);
        return Arrays.asList(qtasks);
    }

    public void update(CreateQueueArg args){
        String id = state().get(CommsRouterResource.QUEUE);
        given()
            .contentType("application/json")
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId", id)
            .body(args)
            .when().post("/routers/{routerId}/queues/{queueId}")
            .then().log().ifError().statusCode(204);
    }
}
