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

import org.junit.*;
import com.softavail.commsrouter.api.dto.arg.*;
import com.softavail.commsrouter.test.api.*;
import static org.hamcrest.Matchers.*;
import com.softavail.commsrouter.api.dto.model.*;

import java.util.HashMap;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;

public class NegativeCasesTest extends BaseTest {
  private static final String longText =  "longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890"+"longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890"+"longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890";
  private static final String utfText =  "longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890";
  
  @Test
  public void createRouterLongName() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    ApiRouter r = new ApiRouter(state);
    r.create(new CreateRouterArg.Builder().name(longText).build())
      .statusCode(500);
  }
  @Test
  public void createRouterLongDescription() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    ApiRouter r = new ApiRouter(state);
    r.create(new CreateRouterArg.Builder().description(longText).build())
      .statusCode(500);
  }
  @Test
  public void createRouterUTFDescription() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    ApiRouter r = new ApiRouter(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build())
      .statusCode(201);
  }

  @Test
  public void deleteRouterWithQueue() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    Queue q = new Queue(state);
    q.create(new CreateQueueArg.Builder()
             .predicate("true")
             .description("desc").build());
    ApiRouter api_r = new ApiRouter(state);
    api_r.delete(state.get(CommsRouterResource.ROUTER))
      .statusCode(500)
      .body("error.description",is("Cannot delete or update 'router' as there is record in 'queue' that refer to it."));
  }
  
  public void deleteRouterWithAgent() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    Agent a = new Agent(state);
    a.create(new CreateAgentArg.Builder("agent").build());
    ApiRouter api_r = new ApiRouter(state);
    api_r.delete(state.get(CommsRouterResource.ROUTER))
      .statusCode(500)
      .body("error.description",is("Cannot delete or update 'router' as there is record in 'agent' that refer to it."));
  }

  @Test
  public void replaceRouterLongRefId() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    ApiRouter r = new ApiRouter(state);
    r.replace(longText,new CreateRouterArg.Builder().description("longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890").build())
      .statusCode(500);
  }

  @Test
  public void queueMissingRouter() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    ApiQueue q = new ApiQueue(state);
    q.create("not-existing-router-ref", 
             new CreateQueueArg.Builder()
             .predicate("true")
             .description("desc").build())
      .statusCode(404)
      .body("error.description", is("Router not-existing-router-ref not found"));
  }

  @Test
  public void agentMissingRouter() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    ApiAgent q = new ApiAgent(state);
    q.create("not-existing-router-ref", 
             new CreateAgentArg.Builder("name")
             .description("desc").build())
      .statusCode(404)
      .body("error.description", is("Router not-existing-router-ref not found"));
  }

  @Test
  public void deleteQueueWithPlan() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    Queue q = new Queue(state);
    q.create(new CreateQueueArg.Builder()
             .predicate("true")
             .description("desc").build());
    Plan p = new Plan(state);
    String defaultQueueId = state.get(CommsRouterResource.QUEUE);
    String predicate = "true";
    
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                                    new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                                    new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());

    ApiQueue api_q = new ApiQueue(state);
    api_q.delete(state.get(CommsRouterResource.ROUTER),state.get(CommsRouterResource.QUEUE))
      .statusCode(500)
      .body("error.description",is("Cannot delete or update 'queue' as there is record in 'route' that refer to it."));
  }

  @Test
  public void deleteQueueWithTask() throws MalformedURLException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    Queue q = new Queue(state);
    q.create(new CreateQueueArg.Builder()
             .predicate("true")
             .description("desc").build());
    Task task = new Task(state);
    task.createQueueTask();

    ApiQueue api_q = new ApiQueue(state);
    api_q.delete(state.get(CommsRouterResource.ROUTER),state.get(CommsRouterResource.QUEUE))
      .statusCode(500)
      .body("error.description",is("Cannot delete or update 'queue' as there is record in 'task' that refer to it."));
  }

  @Test
  public void createQueueWrongPredicate() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    ApiQueue api_q = new ApiQueue(state);
    api_q.create(state.get(CommsRouterResource.ROUTER)
                 , new CreateQueueArg.Builder()
                 .predicate("==true")
                 .description("desc").build())
      .statusCode(400)
      .body("error.description",is("Predicate \"==true\" failed with error: Expression is invalid. "));
  }

  @Test
  public void createQueueWrongPredicateConst() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    ApiQueue api_q = new ApiQueue(state);
    api_q.create(state.get(CommsRouterResource.ROUTER)
                 , new CreateQueueArg.Builder()
                 .predicate("alabala")
                 .description("desc").build())
      .statusCode(400)
      .body("error.description",is("Predicate \"alabala\" failed with error: Expression is invalid. For input string: \"alabala\""));
  }
  
  @Test
  public void createQueueWrongPredicateEmpty() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    ApiQueue api_q = new ApiQueue(state);
    api_q.create(state.get(CommsRouterResource.ROUTER)
                 , new CreateQueueArg.Builder()
                 .predicate("")
                 .description("desc").build())
      .statusCode(400)
      .body("error.description",is("Expression cannot be NULL or empty."));
  }
  
  @Test
  public void createQueueWrongPredicateSubexp() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    ApiQueue api_q = new ApiQueue(state);
    api_q.create(state.get(CommsRouterResource.ROUTER)
                 , new CreateQueueArg.Builder()
                 .predicate("true==[invalidConstant")
                 .description("desc").build())
      .statusCode(400)
      .body("error.description",is("Predicate \"true==[invalidConstant\" failed with error: Expression is invalid. For input string: \"[invalidConstant\""));
  }
  

  @Test
  public void createQueueWrongPredicateSubexp1() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    ApiQueue api_q = new ApiQueue(state);
    api_q.create(state.get(CommsRouterResource.ROUTER)
                 , new CreateQueueArg.Builder()
                 .predicate("true==(invalidConstant)")
                 .description("desc").build())
      .statusCode(400)
      .body("error.description",is("Predicate \"true==(invalidConstant)\" failed with error: Expression is invalid. For input string: \"invalidConstant\""));
  }

  @Test
  public void createQueueWhyPass() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    ApiQueue api_q = new ApiQueue(state);
    api_q.create(state.get(CommsRouterResource.ROUTER)
                 , new CreateQueueArg.Builder()
                 .predicate("true==[invalidConstant]")
                 .description("desc").build())
      .statusCode(201);
  }
  
  @Test
  public void createQueuePredicateSubExpression() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    ApiQueue api_q = new ApiQueue(state);
    api_q.create(state.get(CommsRouterResource.ROUTER)
                 , new CreateQueueArg.Builder()
                 .predicate("10+10==21+(11+[1])")
                 .description("desc").build())
      .statusCode(201);
  }
  
  @Test
  public void deletePlanWithTask() throws MalformedURLException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    Queue q = new Queue(state);
    q.create(new CreateQueueArg.Builder()
             .predicate("true")
             .description("desc").build());

    Plan p = new Plan(state);
    String defaultQueueId = state.get(CommsRouterResource.QUEUE);
    String predicate = "true";
    
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                                    new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                                    new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());

    Task task = new Task(state);
    task.createWithPlan(new CreateTaskArg.Builder()
                        .callback(new URL("http://localhost:8080"))
                        .build());

    ApiPlan api_p = new ApiPlan(state);
    api_p.delete(state.get(CommsRouterResource.ROUTER),state.get(CommsRouterResource.PLAN))
      .statusCode(500)
      .body("error.description",is("Cannot delete or update 'route' as there is record in 'task' that refer to it."));
  }

  @Test
  public void deletePlanWithCanceledTask() throws MalformedURLException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    Queue q = new Queue(state);
    q.create(new CreateQueueArg.Builder()
             .predicate("true")
             .description("desc").build());

    Plan p = new Plan(state);
    String defaultQueueId = state.get(CommsRouterResource.QUEUE);
    String predicate = "true";
    
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                                    new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                                    new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());

    Task task = new Task(state);
    task.createWithPlan(new CreateTaskArg.Builder()
                        .callback(new URL("http://localhost:8080"))
                        .build());
    task.setState(TaskState.canceled);

    ApiPlan api_p = new ApiPlan(state);
    api_p.delete(state.get(CommsRouterResource.ROUTER),state.get(CommsRouterResource.PLAN))
      .statusCode(204);
  }

  @Test
  public void planWithInvalidDefaultQueue() throws MalformedURLException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());

    Plan p = new Plan(state);
    String defaultQueueId = "invalid";
    String predicate = "true";

    Queue q = new Queue(state);
    q.create(new CreateQueueArg.Builder()
             .predicate("true")
             .description("desc").build());
    
    ApiPlan api_p = new ApiPlan(state);

    api_p.create(state.get(CommsRouterResource.ROUTER),
                 new CreatePlanArg.Builder("Rule with predicate " + predicate)
                 .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                                  .routes(Arrays.asList(
                                                                        new RouteDto.Builder(state.get(CommsRouterResource.QUEUE)).timeout(1L)
                                                                        .build(),
                                                                        new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
                 .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
                 .build())
      .statusCode(404)
      .body("error.description",is("Queue " + state.get(CommsRouterResource.ROUTER) + ":invalid not found"));
  }

  @Test
  public void planWithInvalidRuleQueue() throws MalformedURLException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());

    Plan p = new Plan(state);
    String defaultQueueId = "invalid";
    String predicate = "true";

    Queue q = new Queue(state);
    q.create(new CreateQueueArg.Builder()
             .predicate("true")
             .description("desc").build());
    
    ApiPlan api_p = new ApiPlan(state);

    api_p.create(state.get(CommsRouterResource.ROUTER),
                 new CreatePlanArg.Builder("Rule with predicate " + predicate)
                 .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                                  .routes(Arrays.asList(
                                                                        new RouteDto.Builder(defaultQueueId).timeout(1L)
                                                                        .build(),
                                                                        new RouteDto.Builder(state.get(CommsRouterResource.QUEUE)).build()))
                                              .build()))
                 .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
                 .build())
      .statusCode(404)
      .body("error.description",is("Queue " + state.get(CommsRouterResource.ROUTER) + ":invalid not found"));
  }

  @Test
  public void planWithInvalidQueues() throws MalformedURLException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());

    Plan p = new Plan(state);
    String defaultQueueId = "invalid";
    String predicate = "true";
    
    ApiPlan api_p = new ApiPlan(state);

    api_p.create(state.get(CommsRouterResource.ROUTER),
                 new CreatePlanArg.Builder("Rule with predicate " + predicate)
                 .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                                  .routes(Arrays.asList(
                                                                        new RouteDto.Builder(defaultQueueId).timeout(1L)
                                                                        .build(),
                                                                        new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
                 .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
                 .build())
      .statusCode(404)
      .body("error.description",is("Queue " + state.get(CommsRouterResource.ROUTER) + ":invalid not found"));
  }

  @Test
  public void changeTaskStateWaiting2Assigned() throws MalformedURLException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    Queue q = new Queue(state);
    q.create(new CreateQueueArg.Builder()
             .predicate("true")
             .description("desc").build());

    Plan p = new Plan(state);
    String defaultQueueId = state.get(CommsRouterResource.QUEUE);
    String predicate = "true";
    
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                                    new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                                    new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());

    Task task = new Task(state);
    task.createWithPlan(new CreateTaskArg.Builder()
                        .callback(new URL("http://localhost:8080"))
                        .build());
    ApiTask api_t = new ApiTask(state);
    api_t.update(state.get(CommsRouterResource.ROUTER),
                 state.get(CommsRouterResource.TASK),
                 new UpdateTaskArg.Builder().state(TaskState.assigned).build())
      .statusCode(400)
      .body("error.description",is("Expected state: canceled, waiting or completed"));
  }

  @Test
  public void changeTaskStateWaiting2Completed() throws MalformedURLException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    Queue q = new Queue(state);
    q.create(new CreateQueueArg.Builder()
             .predicate("true")
             .description("desc").build());

    Plan p = new Plan(state);
    String defaultQueueId = state.get(CommsRouterResource.QUEUE);
    String predicate = "true";
    
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                                    new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                                    new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());

    Task task = new Task(state);
    task.createWithPlan(new CreateTaskArg.Builder()
                        .callback(new URL("http://localhost:8080"))
                        .build());
    ApiTask api_t = new ApiTask(state);
    api_t.update(state.get(CommsRouterResource.ROUTER),
                 state.get(CommsRouterResource.TASK),
                 new UpdateTaskArg.Builder().state(TaskState.completed).build())
      .statusCode(400)
      .body("error.description",is("Current state cannot be switched to completed: waiting"));
  }

@Test
  public void changeTaskStateWaiting2Waiting() throws MalformedURLException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    Queue q = new Queue(state);
    q.create(new CreateQueueArg.Builder()
             .predicate("true")
             .description("desc").build());

    Plan p = new Plan(state);
    String defaultQueueId = state.get(CommsRouterResource.QUEUE);
    String predicate = "true";
    
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                                    new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                                    new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());

    Task task = new Task(state);
    task.createWithPlan(new CreateTaskArg.Builder()
                        .callback(new URL("http://localhost:8080"))
                        .build());
    ApiTask api_t = new ApiTask(state);
    api_t.update(state.get(CommsRouterResource.ROUTER),
                 state.get(CommsRouterResource.TASK),
                 new UpdateTaskArg.Builder().state(TaskState.waiting).build())
      .statusCode(400)
      .body("error.description",is("Current state cannot be switched to waiting: waiting"));
  }
  
  @Test
  public void createTaskWithInvalidQueue() throws MalformedURLException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    ApiTask api_t = new ApiTask(state);
    api_t.create(state.get(CommsRouterResource.ROUTER),
                 new CreateTaskArg.Builder()
                 .callback(new URL("http://localhost:8080"))
                 .queue("invalid-queue-id")
                 .build())
      .statusCode(404)
      .body("error.description",is("Queue " + state.get(CommsRouterResource.ROUTER) + ":invalid-queue-id not found"));
  }

}
