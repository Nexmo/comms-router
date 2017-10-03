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

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.dto.model.PlanDto;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


public class Plan extends Resource{
    private static final Logger LOGGER = LogManager.getLogger(Plan.class);
    public Plan(HashMap<CommsRouterResource,String> state){
        super(state);
        state.put(CommsRouterResource.PLAN,"id");
        RestAssured.baseURI = System.getProperty("autHost");
        RestAssured.basePath= "/comms-router-web/api";
    }
    public List<PlanDto> list(){
        PlanDto[] routers =given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER)).when()
            .get("/routers/{routerId}/plans")
            .then().statusCode(200)
            .extract().as(PlanDto[].class);
        return Arrays.asList(routers);
    }

    public ApiObject replace(CreatePlanArg args){
        String id = state().get(CommsRouterResource.PLAN);
        ApiObject oid = given()
            .contentType("application/json")
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId", id)
            .body(args)
            .when().put("/routers/{routerId}/plans/{queueId}")
            .then().statusCode(201)
            .extract()
            .as(ApiObject.class);
        state().put(CommsRouterResource.PLAN, oid.getId());
        return oid;
    }

    public ApiObject create(CreatePlanArg args){
        LOGGER.info(given()
                    .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
                    .contentType("application/json")
                    .body(args)
                    .when().post("/routers/{routerId}/plans")
                    .then() .extract().asString());
        ApiObject oid = given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .contentType("application/json")
            .body(args)
            .when().post("/routers/{routerId}/plans")
            .then().statusCode(201).body("id", not(isEmptyString()) )
            .extract()
            .as(ApiObject.class);
        String id=oid.getId();
        state().put(CommsRouterResource.PLAN,id);
        return oid;
    }

    public void delete(){
        String id = state().get(CommsRouterResource.PLAN);
        given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId",id)
            .when().delete("/routers/{routerId}/plans/{queueId}")
            .then().statusCode(204);
    }

    public PlanDto get(){
        String id = state().get(CommsRouterResource.PLAN);
        return given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId",id)
            .when().get("/routers/{routerId}/plans/{queueId}")
            .then().statusCode(200).body("id",equalTo(id))
            .extract().as(PlanDto.class);
    }

    public void update(CreatePlanArg args){
        String id = state().get(CommsRouterResource.PLAN);
        given()
            .contentType("application/json")
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId", id)
            .body(args)
            .when().post("/routers/{routerId}/plans/{queueId}")
            .then().statusCode(204);
    }
}
