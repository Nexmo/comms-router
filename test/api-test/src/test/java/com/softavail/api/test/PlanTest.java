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

import com.softavail.commsrouter.test.api.Queue;
import com.softavail.commsrouter.test.api.Plan;
import com.softavail.commsrouter.test.api.CommsRouterResource;
import com.softavail.commsrouter.test.api.Agent;
import com.softavail.commsrouter.test.api.Skill;
import com.softavail.commsrouter.test.api.Task;
import com.softavail.commsrouter.test.api.Router;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.beans.HasPropertyWithValue.hasProperty;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateSkillArg;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.skill.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import java.util.HashMap;
import java.util.concurrent.TimeUnit;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import org.junit.*;

public class PlanTest extends BaseTest {

    /**
     * Creates a new <code>PlanTest</code> instance.
     *
     */

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Queue q = new Queue(state);
  private Plan p = new Plan(state);
  private Skill s = new Skill(state);
  private Task t = new Task(state);
  private String defaultQueueId;

  @Before
  public void createRouterAndQueue() {
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription("Router description");
    routerArg.setName("router-name");
    ApiObjectRef ref = r.create(routerArg);
    Set<String> options = Stream.of("en","es").collect(Collectors.toSet());
    s.replace("lang", new CreateSkillArg.Builder()
             .name("lang")
             .description("domain")
             .domain( new EnumerationAttributeDomainDto(options))
             .multivalue(false)
             .build());

    String predicate = "lang==en";
    q = new Queue(state);
    defaultQueueId = q.create(new CreateQueueArg.Builder()
                              .predicate(predicate)
                              .description("queue description").build())
        .getRef();
  }

  @After
  public void cleanup() {
    q.delete();
    s.delete();
    r.delete();
  }

  @Test
  public void deletePlanInUse() throws MalformedURLException {
    assertThat(q.size(), is(0));
    String predicate = "lang==en";
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                          new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                          new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());
    t.createWithPlan(new CreateTaskArg.Builder()
                     .callback(new URL("http://localhost:8080"))
                     .requirements(new AttributeGroupDto()
                                   .withKeyValue("lang", new StringAttributeValueDto("en")))
                                   .build());
    p.deleteResponse()
        .statusCode(500)
        .body("error.description",
              equalTo("Cannot delete or update 'route' as there is record in 'task' that refer to it."));
    t.delete();
    p.delete();
    
  }

  @Test
  public void updatePlanInUse() throws MalformedURLException {
    assertThat(q.size(), is(0));
    String predicate = "lang==en";
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                          new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                          new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());
    t.createWithPlan(new CreateTaskArg.Builder()
                     .callback(new URL("http://localhost:8080"))
                     .requirements(new AttributeGroupDto()
                                   .withKeyValue("lang", new StringAttributeValueDto("en")))
                                   .build());
    UpdatePlanArg updateArgs = new UpdatePlanArg();
    updateArgs.setDescription("updated description");
    p.update(updateArgs);
    t.delete();
    p.delete();
  }

  @SuppressWarnings("unchecked")
  @Test
  public void updatePlanForceBackup() throws MalformedURLException {
    assertThat(q.size(), is(0));
    String predicate = "lang==en";
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                          new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                          new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());
    t.createWithPlan(new CreateTaskArg.Builder()
                     .callback(new URL("http://localhost:8080"))
                     .requirements(new AttributeGroupDto()
                                   .withKeyValue("lang", new StringAttributeValueDto("en")))
                                   .build());
    UpdatePlanArg updateArgs = new UpdatePlanArg();
    updateArgs.setDescription("updated description");
    updateArgs.setRules(Collections.singletonList(new RuleDto.Builder(predicate)
                                                  .routes(Arrays.asList(
                                                                        new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                                        new RouteDto.Builder(defaultQueueId).build()))
                                                  .build()));
    
    p.update(updateArgs);
    assertThat(p.list(), hasItems(hasProperty("description", is("updated description"))));
    assertThat(p.list(), hasItems(hasProperty("description", is("Rule with predicate " + predicate))));
    t.delete();
    assertThat(p.list().stream().map((PlanDto dto)-> { p.delete(dto.getRef());return dto;}).count()
               , is(2L));
    
    p.delete();
  }

  @SuppressWarnings("unchecked")
  @Test
  public void updatePlanBackupDescription() throws MalformedURLException {
    assertThat(q.size(), is(0));
    String predicate = "lang==en";
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                          new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                          new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());
    t.createWithPlan(new CreateTaskArg.Builder()
                     .callback(new URL("http://localhost:8080"))
                     .requirements(new AttributeGroupDto()
                                   .withKeyValue("lang", new StringAttributeValueDto("en")))
                                   .build());
    UpdatePlanArg updateArgs = new UpdatePlanArg();
    updateArgs.setRules(Collections.singletonList(new RuleDto.Builder(predicate)
                                                  .routes(Arrays.asList(
                                                                        new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                                        new RouteDto.Builder(defaultQueueId).build()))
                                                  .build()));
    
    p.update(updateArgs);
    assertThat(p.list(), hasItems(hasProperty("description", is("Backup 1 of Rule with predicate " + predicate))));
    assertThat(p.list(), hasItems(hasProperty("description", is("Rule with predicate " + predicate))));
    t.delete();
    assertThat(p.list().stream().map((PlanDto dto)-> { p.delete(dto.getRef());return dto;}).count()
               , is(2L));
    
    p.delete();
  }

  @SuppressWarnings("unchecked")
  @Test
  public void updatePlanBackupMultiple() throws MalformedURLException {
    assertThat(q.size(), is(0));
    String predicate = "lang==en";
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                          new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                          new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());
    t.createWithPlan(new CreateTaskArg.Builder()
                     .callback(new URL("http://localhost:8080"))
                     .requirements(new AttributeGroupDto()
                                   .withKeyValue("lang", new StringAttributeValueDto("en")))
                                   .build());
    UpdatePlanArg updateArgs = new UpdatePlanArg();
    updateArgs.setRules(Collections.singletonList(new RuleDto.Builder(predicate)
                                                  .routes(Arrays.asList(
                                                                        new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                                        new RouteDto.Builder(defaultQueueId).build()))
                                                  .build()));
    
    p.update(updateArgs);
    p.update(updateArgs);
    assertThat(p.list(), hasItems(hasProperty("description", is("Backup 1 of Rule with predicate " + predicate))));
    assertThat(p.list(), hasItems(hasProperty("description", is("Backup 2 of Rule with predicate " + predicate))));
    assertThat(p.list(), hasItems(hasProperty("description", is("Rule with predicate " + predicate))));
    t.delete();
    assertThat(p.list().stream().map((PlanDto dto)-> { p.delete(dto.getRef());return dto;}).count()
               , is(3L));
    
    p.delete();
  }

  @SuppressWarnings("unchecked")
  //@Test TBD Enable when issue is resolved: Update plan failed when plan description is too big #97
  public void updatePlanBigDescription() throws MalformedURLException {
    assertThat(q.size(), is(0));
    String predicate = "lang==en";
    p.create(new CreatePlanArg.Builder("012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678912345")
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                          new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                          new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());
    t.createWithPlan(new CreateTaskArg.Builder()
                     .callback(new URL("http://localhost:8080"))
                     .requirements(new AttributeGroupDto()
                                   .withKeyValue("lang", new StringAttributeValueDto("en")))
                                   .build());
    UpdatePlanArg updateArgs = new UpdatePlanArg();
    updateArgs.setRules(Collections.singletonList(new RuleDto.Builder(predicate)
                                                  .routes(Arrays.asList(
                                                                        new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                                        new RouteDto.Builder(defaultQueueId).build()))
                                                  .build()));
    
    p.update(updateArgs);
    assertThat(p.list(), hasItems(hasProperty("description", is("012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678912345"))));
    assertThat(p.list(), hasItems(hasProperty("description", is("Rule with predicate " + predicate))));
    t.delete();
    assertThat(p.list().stream().map((PlanDto dto)-> { p.delete(dto.getRef());return dto;}).count()
               , is(3L));
    
    p.delete();
  }

}
