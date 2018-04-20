/*
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.api.test;

import com.softavail.commsrouter.test.api.Agent;
import com.softavail.commsrouter.test.api.Queue;
import com.softavail.commsrouter.test.api.ApiQueue;
import com.softavail.commsrouter.test.api.CommsRouterResource;
import com.softavail.commsrouter.test.api.Skill;
import com.softavail.commsrouter.test.api.Task;
import com.softavail.commsrouter.test.api.Router;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.equalTo;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateSkillArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.skill.*;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.*;

/**
 * Unit test for simple App.
 */
// @TestInstance(Lifecycle.PER_CLASS)
//@DisplayName("Queue Test")
public class QueueTest extends BaseTest{

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Skill s = new Skill(state);
  private Queue q = new Queue(state);

  @Before
  public void createRouter() {
    String description = "Router description";
    String name = "router-name";
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription(description);
    routerArg.setName(name);
    ApiObjectRef ref = r.create(routerArg);
    s.replace("language", new CreateSkillArg.Builder()
              .name("language")
              .description("domain")
              .domain( new EnumerationAttributeDomainDto(Stream.of("en","es").collect(Collectors.toSet())))
              .multivalue(false)
              .build());
  }
  
  @After
  public void deleteRouter() {
    q.delete();
    s.delete();
    r.delete();
  }

  @Test
  //@DisplayName("Create new queue.")
  public void createQueue() {
    // best case
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    ApiObjectRef ref = q.create(queueArg);
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(description));
  }

  @Test
  //@DisplayName("Create queue with specified id")
  public void createQueueWithSpecifiedId() {
    // put request to not existing queue
    String description = "queue description";
    String predicate = "1==1";
    String queueRef = "Queue-ref";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    state.put(CommsRouterResource.QUEUE, queueRef);
    state.put(CommsRouterResource.EQUEUE, queueRef);
    ApiObjectRef ref = q.replace(queueArg);
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(description));
    assertThat(queue.getRef(), is(queueRef));
  }

  @Test
  //@DisplayName("Replace existing queue")
  public void replaceExistingQueue() {
    // put request to not existing queue
    String description = "queue description";
    String predicate = "1==1";
    String queueRef = "Queue-id";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    state.put(CommsRouterResource.QUEUE, queueRef);
    state.put(CommsRouterResource.EQUEUE, queueRef);
    
    ApiObjectRef ref = q.replace(queueArg);
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(description));
    assertThat(queue.getRef(), is(queueRef));

    queueArg.setDescription("newDescription");
    queueArg.setPredicate("language==en");

    ref = q.replace(queueArg);
    queue = q.get();
    assertThat(queue.getPredicate(), is("language==en"));
    assertThat(queue.getDescription(), is("newDescription"));
    assertThat(queue.getRef(), is(queueRef));

  }

  @Test
  //@DisplayName("Set parameters")
  public void updateParameters() {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    ApiObjectRef ref = q.create(queueArg);
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(description));

    queueArg.setDescription(null);
    queueArg.setPredicate(null);

    q.update(queueArg);
    queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(description));
    String newDescription = "queue-new-description";
    queueArg.setDescription(newDescription);
    queueArg.setPredicate(null);

    q.update(queueArg);
    queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(newDescription));
    String newPredicate = "language==en";
    queueArg.setDescription(null);
    queueArg.setPredicate(newPredicate);

    q.update(queueArg);
    queue = q.get();
    assertThat(queue.getPredicate(), is(newPredicate));
    assertThat(queue.getDescription(), is(newDescription));
  }

  @Test
  //@DisplayName("empty queue - size is 0")
  public void emptyQueueSize() {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    ApiObjectRef ref = q.create(queueArg);
    assertThat(q.size(), is(0));
  }

  @Test
  //@DisplayName("empty queue - no tasks")
  public void emptyQueueNoTasks() {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    ApiObjectRef ref = q.create(queueArg);
    assertThat(q.size(), is(0));
  }

  @Test
  //@DisplayName("queue with a task")
  public void queueWithTask() throws MalformedURLException {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    ApiObjectRef ref = q.create(queueArg);

    CreateTaskArg targ = new CreateTaskArg();
    targ.setQueueRef(state.get(CommsRouterResource.QUEUE));
    targ.setCallbackUrl(new URL("http://example.com"));
    Task t = new Task(state);
    assertThat(q.size(), is(0));
    t.create(targ);
    assertThat(q.tasks(), hasSize(1));
    assertThat(q.size(), is(1));
    t.delete();
  }

  @Test
  //@DisplayName("it should be allowed to replace queue with tasks")
  public void queueWithTaskReplace() throws MalformedURLException {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    ApiObjectRef ref = q.create(queueArg);

    CreateTaskArg targ = new CreateTaskArg();
    targ.setQueueRef(state.get(CommsRouterResource.QUEUE));
    targ.setCallbackUrl(new URL("http://example.com"));
    Task t = new Task(state);
    assertThat(q.size(), is(0));
    t.create(targ);
    assertThat(q.tasks(), hasSize(1));
    assertThat(q.size(), is(1));

    queueArg.setDescription("qdescription");
    queueArg.setPredicate("1==1");
    ApiQueue api_q = new ApiQueue(state);
    
    api_q.replace(state.get(CommsRouterResource.ROUTER),
                  state.get(CommsRouterResource.QUEUE),
                  queueArg)
      .statusCode(500).body("error.description",
        equalTo("Cannot delete or update 'queue' as there is record in 'task' that refer to it."));
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(description));

    assertThat(q.tasks(), hasSize(1));
    assertThat(q.size(), is(1));

    t.delete();
  }

  @Test
  //@DisplayName("it should be allowed to replace queue with agents")
  public void queueWithAgentReplace() throws MalformedURLException {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    ApiObjectRef ref = q.create(queueArg);

    Agent a = new Agent(state);
    a.create("en");
    assertThat(q.size(), is(0));

    queueArg.setDescription("qdescription");
    queueArg.setPredicate("language==en");

    q.replaceResponse(queueArg).statusCode(201);
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is("language==en"));
    assertThat(queue.getDescription(), is("qdescription"));
    assertThat(queue.getRef(), is(ref.getRef()));
    a.delete();    
  }

  @Test
  public void queueWithPredicateAndNotMatchingTask() throws MalformedURLException {
    String description = "queue description";
    String predicate = "#{department}==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    ApiObjectRef ref = q.create(queueArg);

    CreateTaskArg targ = new CreateTaskArg();
    targ.setQueueRef(state.get(CommsRouterResource.QUEUE));
    targ.setCallbackUrl(new URL("http://example.com"));
    Task t = new Task(state);
    assertThat(q.size(), is(0));
    t.create(targ);
    assertThat(q.tasks(), hasSize(1));
    assertThat(q.size(), is(1));

    queueArg.setDescription("qdescription");
    queueArg.setPredicate("1==something1");

    ApiQueue api_q = new ApiQueue(state);
    
    api_q.replace(state.get(CommsRouterResource.ROUTER),
                  state.get(CommsRouterResource.QUEUE),
                  queueArg)
      .statusCode(500).body("error.description",
        equalTo("Cannot delete or update 'queue' as there is record in 'task' that refer to it."));
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(description));

    assertThat(q.tasks(), hasSize(1));
    assertThat(q.size(), is(1));

    t.delete();
  }
  
}
