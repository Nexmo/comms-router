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

import com.softavail.commsrouter.test.api.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.beans.HasPropertyWithValue.hasProperty;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.*;
import static io.restassured.RestAssured.*;
import io.restassured.response.ValidatableResponse;

import com.softavail.commsrouter.api.dto.arg.*;
import com.softavail.commsrouter.api.dto.model.*;


import org.junit.Test;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Stream;
import java.util.Collections;
import java.util.stream.Collectors;
import com.softavail.commsrouter.api.dto.model.*;
import com.softavail.commsrouter.api.dto.model.skill.*;

/** Unit test for simple App. */
public class AppTest extends BaseTest {
  private Skill s = null;
  private void createSkill(HashMap<CommsRouterResource, String> state) {
    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
                                               new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
                                               new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(50.0,true))
                                               ).collect(Collectors.toList());
    s  = new Skill(state);
    
    s.replace("num", new CreateSkillArg.Builder()
                             .name("num")
                             .description("age domain")
                             .domain( new NumberAttributeDomainDto(intervals))
                             .multivalue(false)
                             .build());
  }
  
  @Test
  public void crudRouter1() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    ApiRouter r = new ApiRouter(state);
    CreateRouterArg arg = new CreateRouterArg();
    arg.setDescription("asdf");
    arg.setName("name");

    ValidatableResponse resp = r.create(arg);

    resp
      .statusCode(201)
      .body("ref", not (isEmptyString()) );
  }

  @Test
  public void crudRouter() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    ApiObjectRef ref = r.create(new CreateRouterArg());
    RouterDto router = r.get();
    assertThat(router.getName(), nullValue());
    r.list();
    //hasItems(hasProperty("ref", is(ref.getRef()))));
    r.replace(new CreateRouterArg());
    r.update(new UpdateRouterArg());
    r.delete();
    r.delete();
  }

  @SuppressWarnings("unchecked")
  @Test
  public void crdQueue() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg());
    createSkill(state);
    
    Queue q = new Queue(state);
    ApiObjectRef ref = q.create(new CreateQueueArg.Builder().predicate("num==1").build());
    QueueDto queue = q.get();
    assertThat(queue.getDescription(), nullValue());
    assertThat(q.list(), hasItems(hasProperty("ref", is(ref.getRef()))));
    q.replace(new CreateQueueArg.Builder().predicate("num==2").build());
    q.update(new CreateQueueArg.Builder().predicate("num==1").build());
    q.delete();
    s.delete();
    r.delete();
  }

  @SuppressWarnings("unchecked")
  @Test // fail when rules is null
  public void crdPlan() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    Plan p = new Plan(state);
    ApiObjectRef ref = r.create(new CreateRouterArg());
    Queue q = new Queue(state);
    ApiObjectRef queueRef = q.create(new CreateQueueArg.Builder().predicate("true").build());
    CreatePlanArg arg = new CreatePlanArg();
    RouteDto defaultRoute = new RouteDto();
    defaultRoute.setQueueRef(queueRef.getRef());
    arg.setDefaultRoute(defaultRoute);
    ref = p.create(arg);
    PlanDto resource = p.get();
    assertThat(resource.getDescription(), nullValue());
    assertThat(p.list(), hasItems(hasProperty("ref", is(ref.getRef()))));
    p.replace(arg);
    UpdatePlanArg updateArg = new UpdatePlanArg();
    p.update(updateArg);
    p.deleteResponse().statusCode(204);
    r.deleteResponse()
      .statusCode(500)
      .body(
            "error.description",
            equalTo(
                    "Cannot delete or update 'router' as there is record in 'queue' that refer to it."));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void crdAgent() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    ApiObjectRef ref = r.create(new CreateRouterArg());
    Agent a = new Agent(state);
    CreateAgentArg arg = new CreateAgentArg();
    ref = a.create(arg);
    AgentDto resource = a.get();
    assertThat(resource.getCapabilities(), nullValue());
    assertThat(a.list(), hasItems(hasProperty("ref", is(ref.getRef()))));
    a.replace(new CreateAgentArg());
    a.update(new UpdateAgentArg());
    a.delete();
    r.delete();
  }

  @SuppressWarnings("unchecked")
  @Test
  public void crdTask() throws MalformedURLException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    ApiObjectRef ref = r.create(new CreateRouterArg());
    Queue q = new Queue(state);
    ApiObjectRef queueRef = q.create(new CreateQueueArg.Builder().predicate("1==1").build());
    Task t = new Task(state);
    CreateTaskArg arg = new CreateTaskArg();
    arg.setCallbackUrl(new URL("http://example.com"));
    arg.setQueueRef(queueRef.getRef());
    ref = t.create(arg);
    TaskDto resource = t.get();
    assertThat(resource.getRequirements(), nullValue());
    assertThat(t.list(), hasItems(hasProperty("ref", is(ref.getRef()))));
    t.replace(
              new CreateTaskArg.Builder()
              .callback(new URL("http://localhost:8080"))
              .queue(queueRef.getRef())
              .build());
    t.update(new UpdateTaskArg.Builder().state(TaskState.canceled).build());
    t.delete();
    q.delete();
    r.delete();
  }
}
