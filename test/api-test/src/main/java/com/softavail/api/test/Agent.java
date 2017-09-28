package com.softavail.api.test;
import com.softavail.api.test.Resource;
import java.util.HashMap;
import java.util.List;
import java.util.Arrays;

import static io.restassured.RestAssured.*;
import static io.restassured.matcher.RestAssuredMatchers.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.dto.model.AgentDto;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


public class Agent extends Resource{
    private static final Logger LOGGER = LogManager.getLogger(Agent.class);
    public Agent(HashMap<CommsRouterResource,String> state){
        super(state);
        state.put(CommsRouterResource.AGENT,"id");
    }
    public List<AgentDto> list(){
        AgentDto[] routers =given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER)).when()
            .get("http://localhost:8080/comms-router-web/api/routers/{routerId}/agents")
            .then().statusCode(200)
            .extract().as(AgentDto[].class);
        return Arrays.asList(routers);
    }

    public ApiObject replace(CreateAgentArg args){
        String id = state().get(CommsRouterResource.AGENT);
        ApiObject oid = given()
            .contentType("application/json")
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId", id)
            .body(args)
            .when().put("http://localhost:8080/comms-router-web/api/routers/{routerId}/agents/{queueId}")
            .then().statusCode(204) // TODO check if 201 is the right code
            .extract()
            .as(ApiObject.class);
        state().put(CommsRouterResource.AGENT, oid.getId());
        return oid;
    }

    public ApiObject create(CreateAgentArg args){

        ApiObject oid = given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .contentType("application/json")
            .body(args)
            .when().post("http://localhost:8080/comms-router-web/api/routers/{routerId}/agents")
            .then().statusCode(201).body("id", not(isEmptyString()) )
            .extract()
            .as(ApiObject.class);
        String id=oid.getId();
        state().put(CommsRouterResource.AGENT,id);
        return oid;
    }

    public void delete(){
        String id = state().get(CommsRouterResource.AGENT);
        given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId",id)
            .when().delete("http://localhost:8080/comms-router-web/api/routers/{routerId}/agents/{queueId}")
            .then().statusCode(204);
    }

    public AgentDto get(){
        String id = state().get(CommsRouterResource.AGENT);
        return given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId",id)
            .when().get("http://localhost:8080/comms-router-web/api/routers/{routerId}/agents/{queueId}")
            .then().statusCode(200).body("id",equalTo(id))
            .extract().as(AgentDto.class);
    }

    public void update(CreateAgentArg args){
        String id = state().get(CommsRouterResource.AGENT);
        given()
            .pathParam("routerId",state().get(CommsRouterResource.ROUTER))
            .pathParam("queueId", id)
            .body(args)
            .when().post("http://localhost:8080/comms-router-web/api/routers/{routerId}/agents/{queueId}")
            .then().statusCode(204);
    }
}
