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
import com.softavail.commsrouter.test.api.Task;
import com.softavail.commsrouter.test.api.Router;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.equalTo;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import com.softavail.commsrouter.test.api.Agent;

/**
 * Unit test for simple App.
 */
// @TestInstance(Lifecycle.PER_CLASS)
@DisplayName("Queue Test")
public class QueueTest {

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);

  @BeforeAll
  public static void beforeAll() throws Exception {
    Assumptions.assumeTrue(System.getProperty("autHost") != null, "autHost is set");
  }

  @BeforeEach
  public void createRouter() {
    // best case
    String description = "Router description";
    String name = "router-name";
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription(description);
    routerArg.setName(name);
    ApiObjectRef ref = r.create(routerArg);
  }

  @Test
  @DisplayName("Create new queue.")
  public void createQueue() {
    // best case
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    Queue q = new Queue(state);
    ApiObjectRef ref = q.create(queueArg);
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(description));
    q.delete();
  }

  @Test
  @DisplayName("Create queue with specified id")
  public void createQueueWithSpecifiedId() {
    // put request to not existing queue
    String description = "queue description";
    String predicate = "1==1";
    String queueRef = "Queue-ref";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    Queue q = new Queue(state);
    state.put(CommsRouterResource.QUEUE, queueRef);
    ApiObjectRef ref = q.replace(queueArg);
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(description));
    assertThat(queue.getRef(), is(queueRef));

    q.delete();
  }

  @Test
  @DisplayName("Replace existing queue")
  public void replaceExistingQueue() {
    // put request to not existing queue
    String description = "queue description";
    String predicate = "1==1";
    String queueRef = "Queue-id";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    Queue q = new Queue(state);
    state.put(CommsRouterResource.QUEUE, queueRef);
    ApiObjectRef ref = q.replace(queueArg);
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(description));
    assertThat(queue.getRef(), is(queueRef));

    queueArg.setDescription("newDescription");
    queueArg.setPredicate("2==2");

    ref = q.replace(queueArg);
    queue = q.get();
    assertThat(queue.getPredicate(), is("2==2"));
    assertThat(queue.getDescription(), is("newDescription"));
    assertThat(queue.getRef(), is(queueRef));

    q.delete();
  }

  @Test
  @DisplayName("Set parameters")
  void updateParameters() {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    Queue q = new Queue(state);
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
    String newPredicate = "2==2";
    queueArg.setDescription(null);
    queueArg.setPredicate(newPredicate);

    q.update(queueArg);
    queue = q.get();
    assertThat(queue.getPredicate(), is(newPredicate));
    assertThat(queue.getDescription(), is(newDescription));

    q.delete();
  }

  @Test
  @DisplayName("empty queue - size is 0")
  void emptyQueueSize() {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    Queue q = new Queue(state);
    ApiObjectRef ref = q.create(queueArg);
    assertThat(q.size(), is(0));
    q.delete();
  }

  @Test
  @DisplayName("empty queue - no tasks")
  void emptyQueueNoTasks() {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    Queue q = new Queue(state);
    ApiObjectRef ref = q.create(queueArg);
    assertThat(q.size(), is(0));
    q.delete();
  }

  @Test
  @DisplayName("queue with a task")
  void queueWithTask() throws MalformedURLException {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    Queue q = new Queue(state);
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
    q.delete();
  }

  @Test
  @DisplayName("it should be allowed to replace queue with tasks")
  void queueWithTaskReplace() throws MalformedURLException {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    Queue q = new Queue(state);
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

    q.replaceResponse(queueArg).statusCode(500).body("error.description",
        equalTo("Cannot delete or update 'queue' as there is record in 'task' that refer to it."));
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is(predicate));
    assertThat(queue.getDescription(), is(description));

    assertThat(q.tasks(), hasSize(1));
    assertThat(q.size(), is(1));

    t.delete();
    q.delete();
  }

  @Test
  @DisplayName("it should not be allowed to replace queue with agents")
  void queueWithAgentReplace() throws MalformedURLException {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    Queue q = new Queue(state);
    ApiObjectRef ref = q.create(queueArg);

    Agent a = new Agent(state);
    a.create("en");
    assertThat(q.size(), is(0));

    queueArg.setDescription("qdescription");
    queueArg.setPredicate("2==2");

    q.replaceResponse(queueArg).statusCode(201);
    QueueDto queue = q.get();
    assertThat(queue.getPredicate(), is("2==2"));
    assertThat(queue.getDescription(), is("qdescription"));
    assertThat(queue.getRef(), is(ref.getRef()));
  }

}
