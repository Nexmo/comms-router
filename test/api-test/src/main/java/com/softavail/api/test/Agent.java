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

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import io.restassured.RestAssured;

public class Agent extends Resource {

  private static final Logger LOGGER = LogManager.getLogger(Agent.class);

  public Agent(HashMap<CommsRouterResource, String> state) {
    super(state);
  }

  public List<AgentDto> list() {
    AgentDto[] routers = given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER)).when()
        .get("/routers/{routerId}/agents")
        .then().statusCode(200)
        .extract().as(AgentDto[].class);
    return Arrays.asList(routers);
  }

  public ApiObjectId createWithId(CreateAgentArg args) {
    String id = state().get(CommsRouterResource.AGENT);
    ApiObjectId oid = given()
        .contentType("application/json")
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .body(args)
        .when().put("/routers/{routerId}/agents/{queueId}")
        .then().statusCode(201)
        .extract()
        .as(ApiObjectId.class);
    state().put(CommsRouterResource.AGENT, oid.getId());
    return oid;
  }

  public ApiObjectId create(CreateAgentArg args) {
    ApiObjectId oid = given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .contentType("application/json")
        .body(args)
        .when().post("/routers/{routerId}/agents")
        .then().statusCode(201).body("id", not(isEmptyString()))
        .extract()
        .as(ApiObjectId.class);
    String id = oid.getId();
    state().put(CommsRouterResource.AGENT, id);
    return oid;
  }

  public ApiObjectId create(String language) {
    CreateAgentArg arg = new CreateAgentArg();
    arg.setAddress("phonenumber");
    arg.setCapabilities(new AttributeGroupDto().withKeyValue("language", new StringAttributeValueDto(language)));
    return create(arg);
  }

  public void delete() {
    String id = state().get(CommsRouterResource.AGENT);
    given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .when().delete("/routers/{routerId}/agents/{queueId}")
        .then().statusCode(204);
  }

  public AgentDto get(String id) {
    return given()
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .when().get("/routers/{routerId}/agents/{queueId}")
        .then().statusCode(200).body("id", equalTo(id))
        .extract().as(AgentDto.class);
  }
  public AgentDto get() {
    return get(state().get(CommsRouterResource.AGENT));
  }

  public void update(UpdateAgentArg args) {
    String id = state().get(CommsRouterResource.AGENT);
    given()
        .contentType("application/json")
        .pathParam("routerId", state().get(CommsRouterResource.ROUTER))
        .pathParam("queueId", id)
        .body(args)
        .when().post("/routers/{routerId}/agents/{queueId}")
        .then().statusCode(204);
  }

  public void setState(AgentState state) {
    UpdateAgentArg agentArg = new UpdateAgentArg();
    agentArg.setState(state);
    update(agentArg);
  }

}
