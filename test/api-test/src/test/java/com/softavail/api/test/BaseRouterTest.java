/*
 * Copyright 2017 SoftAvail, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 *
 */

package com.softavail.api.test;

import com.softavail.commsrouter.test.api.Queue;
import com.softavail.commsrouter.test.api.CommsRouterResource;
import com.softavail.commsrouter.test.api.Router;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.beans.HasPropertyWithValue.hasProperty;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

/**
 * Unit test for simple App.
 */
public class BaseRouterTest {

  @BeforeAll
  public static void beforeAll() throws Exception {
    Assumptions.assumeTrue(System.getProperty("autHost") != null, "autHost is set");
  }

  @Test
  public void createRouter() {
    // best case
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    String description = "Router description";
    String name = "router-name";
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription(description);
    routerArg.setName(name);
    ApiObjectRef ref = r.create(routerArg);
    RouterDto router = r.get();
    assertThat(router.getName(), is(name));
    assertThat(router.getDescription(), is(description));
    r.delete();
  }

  @Test
  public void newRouterWithSpecifiedId() {
    // create router using specified id
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    String description = "Router description";
    String name = "router-name";
    String routerId = "newRouterId";
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription(description);
    routerArg.setName(name);
    state.put(CommsRouterResource.ROUTER, routerId);
    ApiObjectRef ref = r.replace(routerArg);

    RouterDto router = r.get();
    assertThat(router.getName(), is(name));
    assertThat(router.getDescription(), is(description));
    assertThat(router.getRef(), is(routerId));

    r.delete();
  }

  @Test
  public void replaceRouter() {
    // replace existing router
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    String description = "Router description";
    String name = "router-name";
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription(description);
    routerArg.setName(name);
    ApiObjectRef ref = r.create(routerArg);
    RouterDto router = r.get();
    assertThat(router.getName(), is(name));
    assertThat(router.getDescription(), is(description));

    ApiObjectRef ref1 = r.replace(new CreateRouterArg());// replace with null values
    assertThat(ref.getRef(), is(ref1.getRef()));
    router = r.get();
    assertThat(router.getName(), nullValue());
    assertThat(router.getDescription(), nullValue());

    r.delete();
  }

  @SuppressWarnings("unchecked")
  @Test
  public void replaceRouterRouterResourcesAreThere() {
    // replace existing router and check that Queue is here after the replacement
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    String description = "Router description";
    String name = "router-name";
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription(description);
    routerArg.setName(name);
    ApiObjectRef ref = r.create(routerArg);
    RouterDto router = r.get();
    assertThat(router.getName(), is(name));
    assertThat(router.getDescription(), is(description));

    Queue q = new Queue(state);
    ApiObjectRef qRef = q.create(new CreateQueueArg.Builder().predicate("1==1").build());

    r.replaceResponse(new CreateRouterArg()).statusCode(500).body("error.description", equalTo(
        "Cannot delete or update 'router' as there is record in 'queue' that refer to it."));// replace
                                                                                             // with
                                                                                             // null
                                                                                             // values
    // check that queue is still there
    QueueDto queue = q.get();
    assertThat(queue.getDescription(), nullValue());
    assertThat(q.list(), hasItems(hasProperty("ref", is(qRef.getRef()))));

    q.delete();
    r.delete();
  }

  @Test
  public void updateRouterFields() {
    // update values of the existing router
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    String description = "Router description";
    String name = "router-name";
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription(description);
    routerArg.setName(name);
    ApiObjectRef ref = r.create(routerArg);
    RouterDto router = r.get();
    assertThat(router.getName(), is(name));
    assertThat(router.getDescription(), is(description));

    r.update(new CreateRouterArg());// should not change values
    router = r.get();
    assertThat(router.getName(), is(name));
    assertThat(router.getDescription(), is(description));

    routerArg.setDescription("changed");
    routerArg.setName(null);
    r.update(routerArg);// should change only description
    router = r.get();
    assertThat(router.getName(), is(name));
    assertThat(router.getDescription(), is("changed"));

    routerArg.setDescription(null);
    routerArg.setName("changedName");
    r.update(routerArg);// should change only description
    router = r.get();
    assertThat(router.getName(), is("changedName"));
    assertThat(router.getDescription(), is("changed"));

    r.delete();
  }

  @Test
  public void doubleDeleteRouter() {
    // replace existing router
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    String description = "Router description";
    String name = "router-name";
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription(description);
    routerArg.setName(name);
    ApiObjectRef ref = r.create(routerArg);
    RouterDto router = r.get();
    assertThat(router.getName(), is(name));
    assertThat(router.getDescription(), is(description));
    r.delete();
    r.delete();
  }

}
