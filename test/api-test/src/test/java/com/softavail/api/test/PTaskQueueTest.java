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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.concurrent.TimeUnit;

/**
 * Unit test for Task to queue mapping.
 */
@DisplayName("Task to Queue mapping Tests")
public class PTaskQueueTest {

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Queue q = new Queue(state);
  private Plan p = new Plan(state);
  private Task t = new Task(state);
  private String defaultQueueId;
  private String backupQueueId;
  private String mainQueueId;
  @BeforeEach
  public void createRouterAndQueue() {
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription("Router description");
    routerArg.setName("router-name");
    ApiObjectId id = r.create(routerArg);

    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription("queue description");
    queueArg.setPredicate(predicate);
    q = new Queue(state);
    defaultQueueId = q.create(new CreateQueueArg.Builder()
        .predicate(predicate)
        .description("queue description").build())
        .getId();

    backupQueueId = q.create(new CreateQueueArg.Builder()
        .predicate(predicate)
        .description("backup queue description").build())
        .getId();

    mainQueueId = q.create(new CreateQueueArg.Builder()
                  .predicate(predicate)
                  .description("queue description").build())
        .getId();
  }

    //@AfterEach
  public void cleanup() {
    t.delete();
    p.delete();
    q.delete();
    r.delete();
  }

  private void addPlanTask(AttributeGroupDto requirements, String predicate)
      throws MalformedURLException {
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
               .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                                .routes(Arrays.asList(
                                                            new RouteDto.Builder(mainQueueId).timeout(1L).build(),
                                                            new RouteDto.Builder(backupQueueId).build()))
                                                .build()))
               .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
               .build());
    t.createWithPlan(new CreateTaskArg.Builder()
                     .callback(new URL("http://localhost:8080"))
                     .requirements(requirements)
                     .build() );
  }

  @Test
  @DisplayName("Add task with one attribute to queue.")
  public void addTaskOneAttribute() throws MalformedURLException {
    assertThat(q.size(), is(0));
    addPlanTask(new AttributeGroupDto()
            .withKeyValue("lang", new StringAttributeValueDto("en"))
        , "1==1");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate HAS with single item")
  public void addTaskHasOneItemExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    addPlanTask(new AttributeGroupDto()
            .withKeyValue("age", new DoubleAttributeValueDto(20))
        , "HAS([10],#{age})");
    assertThat(q.size(), is(0));
    state.put(CommsRouterResource.QUEUE, defaultQueueId);
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate HAS with no items")
  public void addTaskHasNoItemExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    addPlanTask(new AttributeGroupDto()
            .withKeyValue("age", new DoubleAttributeValueDto(20))
        , "HAS([],#{age})");
    assertThat(q.size(), is(0));
    state.put(CommsRouterResource.QUEUE, defaultQueueId);
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with timed out queue")
  public void addTaskTimedoutQueue() throws MalformedURLException, InterruptedException {
    assertThat(q.size(), is(0));
    addPlanTask(new AttributeGroupDto()
            .withKeyValue("age", new DoubleAttributeValueDto(20))
        , "HAS([20],#{age})");
    assertThat(q.size(), is(1));
    TimeUnit.SECONDS.sleep(2);
    assertThat(String.format("Router %s. Check task is not in the queue after the timeout.",
        state.get(CommsRouterResource.ROUTER)),
        q.size(), is(0));
    state.put(CommsRouterResource.QUEUE, backupQueueId);
    assertThat(q.size(), is(1));
  }

}
