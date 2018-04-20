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
import com.softavail.commsrouter.api.dto.model.skill.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

import java.net.ServerSocket;
import java.net.Socket;
import java.io.OutputStreamWriter;
import java.io.IOException;

import java.util.HashMap;
import java.util.List;
import java.util.stream.Stream;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import javax.ws.rs.core.HttpHeaders;

public class NegativeCasesTest extends BaseTest {
  private static final String longText =  "longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890"+"longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890"+"longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890";
  private static final String utfText =  "longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890";
  private ServerSocket server;

  private String waitToConnect(int timeout) throws IOException {

    server.setSoTimeout(timeout);
      
    Socket socket = server.accept();
    OutputStreamWriter ow = new OutputStreamWriter(socket.getOutputStream(),"UTF-8");
    ow.write("HTTP1/0 200 OK\r\n\r\n");
    String result = (new BufferedReader(new InputStreamReader(socket.getInputStream()))
                     .lines().collect(Collectors.joining("\n")));
    return result ;
  }

  private URL testServer() throws MalformedURLException {
    String host = (System.getProperty("runTestsOn")!=null) ? System.getProperty("runTestsOn") : "http://localhost";
    return new URL( host + ":" +  server.getLocalPort());
  }

  @Before
  public void setup() throws IOException {
    server = new ServerSocket(0);
  }

  @After
  public void cleanup() throws IOException {
    server.close();
  }

  
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
    Skill s = new Skill(state);
    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
                                               new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
                                               new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(50.0,true))
                                               ).collect(Collectors.toList());

    s.replace("true", new CreateSkillArg.Builder()
              .name("true")
              .description("age domain")
              .domain( new NumberAttributeDomainDto(intervals))
              .multivalue(false)
              .build());

    ApiQueue api_q = new ApiQueue(state);
    api_q.create(state.get(CommsRouterResource.ROUTER)
                 , new CreateQueueArg.Builder()
                 .predicate("==true")
                 .description("desc").build())
      .statusCode(400)
      .body("error.description",startsWith("Invalid expression: cz.jirutka.rsql.parser.ParseException: Encountered "));
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
      .body("error.description",startsWith("Invalid expression: cz.jirutka.rsql.parser.ParseException: "));
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
      .statusCode(201)
      .body("ref",notNullValue());
  }
  
  @Test
  public void createQueueWrongPredicateSubexp() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    Skill s = new Skill(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
                                               new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
                                               new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(50.0,true))
                                               ).collect(Collectors.toList());

    s.replace("true", new CreateSkillArg.Builder()
              .name("true")
              .description("age domain")
              .domain( new NumberAttributeDomainDto(intervals))
              .multivalue(false)
              .build());

    ApiQueue api_q = new ApiQueue(state);
    api_q.create(state.get(CommsRouterResource.ROUTER)
                 , new CreateQueueArg.Builder()
                 .predicate("true==[invalidConstant")
                 .description("desc").build())
      .statusCode(400).body("error.description", containsString("Invalid number argument:[invalidConstant"));
  }
  
  @Test
  public void createQueueWrongPredicateSubexp1() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    Skill s = new Skill(state);
    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
                                               new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
                                               new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(50.0,true))
                                               ).collect(Collectors.toList());

    s.replace("true", new CreateSkillArg.Builder()
              .name("true")
              .description("true domain")
              .domain( new NumberAttributeDomainDto(intervals))
              .multivalue(false)
              .build());

    ApiQueue api_q = new ApiQueue(state);
    api_q.create(state.get(CommsRouterResource.ROUTER)
                 , new CreateQueueArg.Builder()
                 .predicate("true==(invalidConstant)")
                 .description("desc").build())
      .statusCode(400)
      .body("error.description", containsString("Invalid number argument:invalidConstant"));
  }

  @Test
  public void createQueueWhyPass() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());

    Skill s = new Skill(state);
    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
                                               new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
                                               new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(50.0,true))
                                               ).collect(Collectors.toList());

    s.replace("true", new CreateSkillArg.Builder()
              .name("true")
              .description("age domain")
              .domain( new NumberAttributeDomainDto(intervals))
              .multivalue(false)
              .build());
    ApiQueue api_q = new ApiQueue(state);
    api_q.create(state.get(CommsRouterResource.ROUTER)
                 , new CreateQueueArg.Builder()
                 .predicate("true==[invalidConstant]")
                 .description("desc").build())
      .statusCode(400) 
      .body("error.description", containsString("Invalid number argument:[invalidConstant]"));
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
      .statusCode(400).body("error.description", containsString("Invalid expression: cz.jirutka.rsql.parser.ParseException: Encountered "));
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

  @Test
  public void changeStateOfBusyAgent() throws MalformedURLException, IOException {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build());
    Agent a = new Agent(state);
    a.create(new CreateAgentArg());
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
                        .callback(testServer())
                        .build());
    AgentDto resource = a.get();
    assertThat(String.format("Check state (%s) to be offline.", resource.getState()),
               resource.getState(), is(AgentState.offline));
    a.setState(AgentState.ready);
    assertThat(waitToConnect(3000), allOf(containsString(state.get(CommsRouterResource.AGENT)),
                                          containsString((state.get(CommsRouterResource.TASK)))));

    resource = a.get();
    assertThat(String.format("Check state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));
    ApiAgent api_a = new ApiAgent(state);
    String etag = api_a.update(state.get(CommsRouterResource.EAGENT),
                               state.get(CommsRouterResource.ROUTER),
                               state.get(CommsRouterResource.AGENT),
                               new UpdateAgentArg.Builder().state(AgentState.ready).build())
      .statusCode(400)
      .body("error.description",is("Changing state of a busy agent is not implemented. Complete corresponding task."))
      .extract().header(HttpHeaders.ETAG);
    assertThat(etag,equalTo(null));
    
    etag = api_a.update(state.get(CommsRouterResource.EAGENT),
                 state.get(CommsRouterResource.ROUTER),
                 state.get(CommsRouterResource.AGENT),
                 new UpdateAgentArg.Builder().state(AgentState.offline).build())
      .statusCode(400)
      .body("error.description",is("Changing state of a busy agent is not implemented. Complete corresponding task."))
      .extract().header(HttpHeaders.ETAG);
    assertThat(etag,equalTo(null));

    etag = api_a.update(state.get(CommsRouterResource.EAGENT),
                 state.get(CommsRouterResource.ROUTER),
                 state.get(CommsRouterResource.AGENT),
                 new UpdateAgentArg.Builder().state(AgentState.unavailable).build())
      .statusCode(400)
      .body("error.description",is("Setting agent state to 'unavailable' not allowed"))
      .extract().header(HttpHeaders.ETAG);
    assertThat(etag,equalTo(null));
    
    etag = api_a.update(state.get(CommsRouterResource.EAGENT),
                 state.get(CommsRouterResource.ROUTER),
                 state.get(CommsRouterResource.AGENT),
                 new UpdateAgentArg.Builder().state(AgentState.busy).build())
      .statusCode(400)
      .body("error.description",is("Setting agent state to 'busy' not allowed"))
      .extract().header(HttpHeaders.ETAG);
    assertThat(etag,equalTo(null));
    
  }
}
