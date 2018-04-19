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
import com.softavail.commsrouter.test.api.Skill;
import com.softavail.commsrouter.test.api.CommsRouterResource;
import com.softavail.commsrouter.test.api.Task;
import com.softavail.commsrouter.test.api.Router;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import org.junit.*;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.dto.arg.CreateSkillArg;
import com.softavail.commsrouter.api.dto.model.skill.*;
import java.util.Set;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.concurrent.TimeUnit;

/** Unit test for Task to queue mapping. */
// @DisplayName("Task to Queue mapping Tests")
public class PTaskQueueTest extends BaseTest {

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Queue q = new Queue(state);
  private Plan p = new Plan(state);
  private Skill s = new Skill(state);
  private Task t = new Task(state);
  private String defaultQueueId;
  private String backupQueueId;
  private String mainQueueId;

  @Before
  public void createRouterAndQueue() {
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription("Router description");
    routerArg.setName("router-name");
    ApiObjectRef ref = r.create(routerArg);

    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
                                               new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
                                               new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(50.0,true))
                                               ).collect(Collectors.toList());

    s.replace("age", new CreateSkillArg.Builder()
              .name("age")
              .description("age domain")
              .domain( new NumberAttributeDomainDto(intervals))
              .multivalue(false)
              .build());

    Set<String> options = Stream.of("en","es").collect(Collectors.toSet());
    s.replace("lang", new CreateSkillArg.Builder()
              .name("lang")
              .description("language domain")
              .domain( new EnumerationAttributeDomainDto(options))
              .multivalue(false)
              .build());

    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription("queue description");
    queueArg.setPredicate(predicate);
    q = new Queue(state);
    defaultQueueId =
        q.create(
                new CreateQueueArg.Builder()
                    .predicate(predicate)
                    .description("queue description")
                    .build())
            .getRef();

    backupQueueId =
        q.create(
                new CreateQueueArg.Builder()
                    .predicate(predicate)
                    .description("backup queue description")
                    .build())
            .getRef();

    mainQueueId =
        q.create(
                new CreateQueueArg.Builder()
                    .predicate(predicate)
                    .description("queue description")
                    .build())
            .getRef();
  }

  @After
  public void cleanup() {
    t.delete();
    p.delete();
    assertThat(q.list().stream().map((QueueDto dto)-> { q.delete(dto.getRef());return dto;}).count()
               , is(3L));
    assertThat(s.list().stream().map((SkillDto dto)-> { s.delete(dto.getRef());return dto;}).count()
               , is(2L));
    r.delete();
  }

  private void addPlanTask(AttributeGroupDto requirements, String predicate)
      throws MalformedURLException {
    p.create(
        new CreatePlanArg.Builder("Rule with predicate " + predicate)
            .rules(
                Collections.singletonList(
                    new RuleDto.Builder(predicate)
                        .routes(
                            Arrays.asList(
                                new RouteDto.Builder(mainQueueId).timeout(1L).build(),
                                new RouteDto.Builder(backupQueueId).build()))
                        .build()))
            .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
            .build());
    t.createWithPlan(
        new CreateTaskArg.Builder()
            .callback(new URL("http://localhost:8080"))
            .requirements(requirements)
            .build());
  }

  @Test
  // @DisplayName("Add task with one attribute to queue.")
  public void addTaskOneAttribute() throws MalformedURLException {
    assertThat(q.size(), is(0));
    addPlanTask(
        new AttributeGroupDto().withKeyValue("lang", new StringAttributeValueDto("en")), "1==1");
    assertThat(q.size(), is(1));
  }

  @Test
  // @DisplayName("Add task with one attribute and predicate HAS with single item")
  public void addTaskHasOneItemExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    addPlanTask(
        new AttributeGroupDto().withKeyValue("age", new DoubleAttributeValueDto(20)),
        "HAS([10],#{age})");
    assertThat(q.size(), is(0));
    state.put(CommsRouterResource.QUEUE, defaultQueueId);
    assertThat(q.size(), is(1));
  }

  @Test
  // @DisplayName("Add task with one attribute and predicate HAS with no items")
  public void addTaskHasNoItemExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    addPlanTask(
        new AttributeGroupDto().withKeyValue("age", new DoubleAttributeValueDto(20)),
        "HAS([],#{age})");
    assertThat(q.size(), is(0));
    state.put(CommsRouterResource.QUEUE, defaultQueueId);
    assertThat(q.size(), is(1));
  }

  @Test
  // @DisplayName("Add task with timed out queue")
  public void addTaskTimedoutQueue() throws MalformedURLException, InterruptedException {
    assertThat(q.size(), is(0));
    addPlanTask(
        new AttributeGroupDto().withKeyValue("age", new DoubleAttributeValueDto(20)),
        "HAS([20],#{age})");
    assertThat(q.size(), is(1));
    TimeUnit.SECONDS.sleep(2);
    assertThat(
        String.format(
            "Router %s. Check task is not in the queue after the timeout.",
            state.get(CommsRouterResource.ROUTER)),
        q.size(),
        is(0));
    state.put(CommsRouterResource.QUEUE, backupQueueId);
    assertThat(q.size(), is(1));
  }
}
