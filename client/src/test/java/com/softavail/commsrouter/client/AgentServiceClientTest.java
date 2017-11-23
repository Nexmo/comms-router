package com.softavail.commsrouter.client;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.put;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.junit.WireMockClassRule;
import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;

import java.util.UUID;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;

/**
 * Created by @author mapuo on 07.10.17.
 */
public class AgentServiceClientTest {

  private static final Logger LOGGER = LogManager.getLogger(AgentServiceClientTest.class);

  @ClassRule
  public static WireMockClassRule wireMockRule =
      new WireMockClassRule(wireMockConfig().dynamicPort());

  @Rule
  public WireMockClassRule instance = wireMockRule;

  private static String endpoint;

  private static Client client;

  private static String routerRef;

  private static AgentServiceClient serviceClient;

  private static ObjectMapper objectMapper;

  @BeforeClass
  public static void setUpClass() throws Exception {
    endpoint = "http://localhost:" + wireMockRule.port() + "/api";
    client = ClientBuilder.newClient();
    routerRef = UUID.randomUUID().toString();
    serviceClient = new AgentServiceClient(client, endpoint, routerRef);
    objectMapper = new ObjectMapper();
  }

  @Before
  public void setUp() throws Exception {
  }

  @Test
  public void create() throws Exception {
    String agentRef = UUID.randomUUID().toString();

    stubFor(post(urlEqualTo("/api/routers/" + routerRef + "/agents"))
        .withHeader("Accept", equalTo("application/json"))
        .willReturn(aResponse()
            .withStatus(201)
            .withHeader("Content-Type", "application/json")
            .withBody("{\"ref\":\"" + agentRef + "\"}")));

    CreateAgentArg createArg = new CreateAgentArg();
    createArg.setAddress("sip:someone@somesip.pip");
    ApiObjectRef apiObjectId = serviceClient.create(createArg, routerRef);

    LOGGER.debug("apiObjectId: {}", apiObjectId);

    assertEquals("agentRef", agentRef, apiObjectId.getRef());
  }

  @Test
  public void createWithId() throws Exception {
    String agentRef = UUID.randomUUID().toString();

    stubFor(put(urlEqualTo("/api/routers/" + routerRef + "/agents/" + agentRef))
        .withHeader("Accept", equalTo("application/json"))
        .willReturn(aResponse()
            .withStatus(201)
            .withHeader("Content-Type", "application/json")
            .withBody("{\"ref\":\"" + agentRef + "\"}")));

    CreateAgentArg createArg = new CreateAgentArg();
    createArg.setAddress("sip:someone@somesip.pip");
    ApiObjectRef apiObjectRef =
        serviceClient.replace(createArg, new RouterObjectRef(agentRef, routerRef));

    LOGGER.debug("apiObjectId: {}", apiObjectRef);

    assertEquals("agentRef", agentRef, apiObjectRef.getRef());
  }

  @Test
  public void update() throws Exception {
    String agentId = UUID.randomUUID().toString();

    stubFor(put(urlEqualTo("/api/routers/" + routerRef + "/agents/" + agentId))
        .withHeader("Accept", equalTo("application/json"))
        .willReturn(aResponse()
            .withStatus(204)
            .withHeader("Content-Type", "application/json")));

    UpdateAgentArg updateAgentArg = new UpdateAgentArg();
    updateAgentArg.setAddress("sip:someone@somesip.pip");
    serviceClient.update(updateAgentArg, new RouterObjectRef(agentId, routerRef));
  }

  @Test
  public void get() throws Exception {
    String agentId = UUID.randomUUID().toString();
    AgentDto agent = new AgentDto();
    agent.setAddress("sip:someone@somesip.pip");

    stubFor(WireMock.get(urlEqualTo("/api/routers/" + routerRef + "/agents/" + agentId))
        .withHeader("Accept", equalTo("application/json"))
        .willReturn(aResponse()
            .withStatus(200)
            .withHeader("Content-Type", "application/json")
        .withBody(objectMapper.writeValueAsString(agent))));

    AgentDto agentDto = serviceClient.get(new RouterObjectRef(agentId, routerRef));

    assertNotNull("agent is found",
        agentDto);

    assertEquals("address matches",
        agent.getAddress(), agentDto.getAddress());
  }

  @Test
  public void list() throws Exception {
  }

  @Test
  public void listPage() throws Exception {
  }

  @Test
  public void delete() throws Exception {
    String agentRef = UUID.randomUUID().toString();

    stubFor(WireMock.delete(urlEqualTo("/api/routers/" + routerRef + "/agents/" + agentRef))
        .withHeader("Accept", equalTo("application/json"))
        .willReturn(aResponse()
            .withStatus(204)
            .withHeader("Content-Type", "application/json")));

    serviceClient.delete(new RouterObjectRef(agentRef, routerRef));
  }

}
