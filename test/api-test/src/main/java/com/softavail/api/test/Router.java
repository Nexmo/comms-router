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

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.dto.model.RouterDto;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


public class Router extends Resource{
    private static final Logger LOGGER = LogManager.getLogger(Router.class);
    public Router(HashMap<CommsRouterResource,String> state){
        super(state);
        state.put(CommsRouterResource.ROUTER,"id");
        RestAssured.baseURI = System.getProperty("autHost");
        RestAssured.basePath= "/comms-router-web/api/";
    }
    public List<RouterDto> list(){
        RouterDto[] routers =given().when()
            .get("/routers")
            .then().statusCode(200)
            .extract().as(RouterDto[].class);
        return Arrays.asList(routers);
    }

    public ApiObject replace(CreateRouterArg args){
        String id = state().get(CommsRouterResource.ROUTER);
        ApiObject oid = given()
            .contentType("application/json")
            .pathParam("routerId", id)
            .body(args)
            .when().put("/routers/{routerId}")
            .then().statusCode(201)
            .extract()
            .as(ApiObject.class);
        state().put(CommsRouterResource.ROUTER, oid.getId());
        return oid;
    }

    public ApiObject create(CreateRouterArg args){

        ApiObject oid = given()
            .contentType("application/json")
            .body(args)
            .when().post("/routers")
            .then().statusCode(201).body("id", not(isEmptyString()) )
            .extract()
            .as(ApiObject.class);
        String id=oid.getId();
        state().put(CommsRouterResource.ROUTER,id);
        return oid;
    }

    public void delete(){
        String id = state().get(CommsRouterResource.ROUTER);
        given()
            .pathParam("routerId",id)
            .when().delete("/routers/{routerId}")
            .then().statusCode(204);
    }

    public RouterDto get(){
        String id = state().get(CommsRouterResource.ROUTER);
        return given()
            .pathParam("routerId",id)
            .when().get("/routers/{routerId}")
            .then().statusCode(200).body("id",equalTo(id))
            .extract().as(RouterDto.class);
    }

    public void update(CreateRouterArg args){
        String id = state().get(CommsRouterResource.ROUTER);
        given()
            .contentType("application/json")
            .pathParam("routerId", id)
            .body(args)
            .when().post("/routers/{routerId}")
            .then().statusCode(204);
    }
}
