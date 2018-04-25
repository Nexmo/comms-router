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

package com.softavail.api.test;

import com.softavail.commsrouter.api.dto.arg.*;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.skill.EnumerationAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberInterval;
import com.softavail.commsrouter.api.dto.model.skill.NumberIntervalBoundary;
import com.softavail.commsrouter.test.api.Queue;
import com.softavail.commsrouter.test.api.Plan;
import com.softavail.commsrouter.test.api.CommsRouterResource;
import com.softavail.commsrouter.test.api.Agent;
import com.softavail.commsrouter.test.api.ApiAgent;
import com.softavail.commsrouter.test.api.Task;
import com.softavail.commsrouter.test.api.Router;
import com.softavail.commsrouter.test.api.Skill;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.allOf;
import org.junit.*;

import com.softavail.commsrouter.api.dto.model.AgentDto;

import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.concurrent.TimeUnit;
import java.util.Set;
import java.util.List;

import java.net.Socket;
import java.util.Collections;

import java.io.OutputStreamWriter;
import org.hamcrest.Matchers;
import org.hamcrest.MatcherAssert;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.stream.Collectors;
import java.io.BufferedReader;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Unit test for Agents.
 */

public class AgentSkillValidateTest extends BaseTest {

  private static final Logger LOGGER = LogManager.getLogger(Agent.class);

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Queue q = new Queue(state);
  private Agent a = new Agent(state);
  private Task t = new Task(state);
  private Plan p = new Plan(state);

  @Before
  public void setup() throws IOException {
    r.create(new CreateRouterArg());
  }

  @After
  public void cleanup() throws IOException {
    r.delete();
  }

  @Test
  public void createAgentMissingSkillName() {
    ApiAgent api_a = new ApiAgent(state);

    api_a.create(state.get(CommsRouterResource.ROUTER),
                 new CreateAgentArg.Builder("capabilities")
                 .capabilities(
                               new AttributeGroupDto()
                               .withKeyValue("missing",
                                             new StringAttributeValueDto("missing")))
                 .build())
      .statusCode(400).body("error.description", is("Skill missing was not found."));

    assertThat(a.list().stream().map((AgentDto dto)-> { a.delete(dto.getRef());return dto;}).count(), is(0L));
  }

  @Test
  public void createAgentMissingSkillValue() {
    Skill s = new Skill(state);
    Set<String> options;
    options = Stream.of("en","es").collect(Collectors.toSet());
    s.replace("language",
            new CreateSkillArg.Builder()
                    .name("language")
                    .description("domain")
                    .domain( new EnumerationAttributeDomainDto(options))
                    .multivalue(false)
                    .build());

    ApiAgent api_a = new ApiAgent(state);

    api_a.create(state.get(CommsRouterResource.ROUTER),
            new CreateAgentArg.Builder("capabilities")
                    .capabilities(
                            new AttributeGroupDto()
                                    .withKeyValue("language",
                                            new StringAttributeValueDto("missing")))
                    .build())
            .statusCode(400).body("error.description", is("Invalid value for skill language: missing"));

    s.delete();
    assertThat(a.list().stream().map((AgentDto dto)-> { a.delete(dto.getRef());return dto;}).count(),
            is(0L));
  }

  @Test
  public void createAgentMissingSkillNumValue() {
    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
            new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
            new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(50.0,true))
    ).collect(Collectors.toList());
    Skill s  = new Skill(state);

    s.replace("num", new CreateSkillArg.Builder()
            .name("num")
            .description("age domain")
            .domain( new NumberAttributeDomainDto(intervals))
            .multivalue(false)
            .build());

    ApiAgent api_a = new ApiAgent(state);

    api_a.create(state.get(CommsRouterResource.ROUTER),
            new CreateAgentArg.Builder("capabilities")
                    .capabilities(
                            new AttributeGroupDto()
                                    .withKeyValue("num",
                                            new StringAttributeValueDto("missing")))
                    .build())
            .statusCode(400).body("error.description", is("Invalid value for skill num: missing"));
    s.delete();
    assertThat(a.list().stream().map((AgentDto dto)-> { a.delete(dto.getRef());return dto;}).count(),
            is(0L));
  }

  @Test
  public void createAgentMissingSkillNumOutOfRange() {
    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
            new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
            new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(50.0,true))
    ).collect(Collectors.toList());
    Skill s  = new Skill(state);

    s.replace("num", new CreateSkillArg.Builder()
            .name("num")
            .description("age domain")
            .domain( new NumberAttributeDomainDto(intervals))
            .multivalue(false)
            .build());

    ApiAgent api_a = new ApiAgent(state);

    api_a.create(state.get(CommsRouterResource.ROUTER),
            new CreateAgentArg.Builder("capabilities")
                    .capabilities(
                            new AttributeGroupDto()
                                    .withKeyValue("num",
                                            new DoubleAttributeValueDto(0)))
                    .build())
            .statusCode(400).body("error.description",
              is("Invalid value for skill num: 0.0. Accepted intervals are [(1.0, 2.0), (2.0, 3.0), (4.0, 50.0]]"));
    s.delete();
    assertThat(a.list().stream().map((AgentDto dto)-> { a.delete(dto.getRef());return dto;}).count(),
            is(0L));
  }


  @Test
  public void createAgentMissingSkillNumOutOfRangeBig() {
    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
            new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
            new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(50.0,true))
    ).collect(Collectors.toList());
    Skill s  = new Skill(state);

    s.replace("num", new CreateSkillArg.Builder()
            .name("num")
            .description("age domain")
            .domain( new NumberAttributeDomainDto(intervals))
            .multivalue(false)
            .build());

    ApiAgent api_a = new ApiAgent(state);

    api_a.create(state.get(CommsRouterResource.ROUTER),
            new CreateAgentArg.Builder("capabilities")
                    .capabilities(
                            new AttributeGroupDto()
                                    .withKeyValue("num",
                                            new DoubleAttributeValueDto(110)))
                    .build())
            .statusCode(400).body("error.description",
            is("Invalid value for skill num: 110.0. Accepted intervals are [(1.0, 2.0), (2.0, 3.0), (4.0, 50.0]]"));
    s.delete();
    assertThat(a.list().stream().map((AgentDto dto)-> { a.delete(dto.getRef());return dto;}).count(),
            is(0L));
  }

}
