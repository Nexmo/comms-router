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
import com.softavail.commsrouter.test.api.Task;
import com.softavail.commsrouter.test.api.Router;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.beans.HasPropertyWithValue.hasProperty;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;

/**
 * Unit test for simple App.
 */
public class AppTest {

  @BeforeAll
  public static void beforeAll() throws Exception {
    Assumptions.assumeTrue(System.getProperty("autHost") != null, "autHost is set");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void crudRouter() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    ApiObjectRef ref = r.create(new CreateRouterArg());
    RouterDto router = r.get();
    assertThat(router.getName(), nullValue());
    r.list(); // @todo: fix this to consider pagination: assertThat(r.list(), hasItems(hasProperty("ref", is(ref.getRef()))));
    r.replace(new CreateRouterArg());
    r.update(new CreateRouterArg());
    r.delete();
    r.delete();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void crdQueue() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg());

    Queue q = new Queue(state);
    ApiObjectRef ref = q.create(new CreateQueueArg.Builder().predicate("1==1").build());
    QueueDto queue = q.get();
    assertThat(queue.getDescription(), nullValue());
    assertThat(q.list(), hasItems(hasProperty("ref", is(ref.getRef()))));
    q.replace(new CreateQueueArg.Builder().predicate("2==2").build());
    q.update(new CreateQueueArg.Builder().predicate("1==1").build());
    q.delete();
    r.delete();
  }

  @Test // fail when rules is null
  @SuppressWarnings("unchecked")
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
    p.deleteResponse()
        .statusCode(204);
    r.deleteResponse()
        .statusCode(500)
        .body("error.description",
              equalTo("Cannot delete or update 'router' as there is record in 'queue' that refer to it."));
  }

  @Test
  @SuppressWarnings("unchecked")
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

  @Test
  @SuppressWarnings("unchecked")
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
    t.replace(new CreateTaskArg.Builder().callback(new URL("http://localhost:8080"))
        .queue(queueRef.getRef()).build());
    t.update(new UpdateTaskArg.Builder().state(TaskState.canceled).build());
    t.delete();
    q.delete();
    r.delete();
  }

}
