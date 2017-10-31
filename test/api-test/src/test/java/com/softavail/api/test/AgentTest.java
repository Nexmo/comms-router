package com.softavail.api.test;

import static io.restassured.RestAssured.*;
import static io.restassured.matcher.RestAssuredMatchers.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.beans.HasPropertyWithValue.hasProperty;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.DisplayName;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import java.util.HashMap;
import java.util.Arrays;
import java.net.URL;
import java.net.MalformedURLException;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import java.util.concurrent.TimeUnit;
/**
 * Unit test for simple App.
 */
// @TestInstance(Lifecycle.PER_CLASS)
@DisplayName("Agent Test")
public class AgentTest {

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Queue q = new Queue(state);
  private Agent a = new Agent(state);
  private Task t = new Task(state);

  public void createRouter() {
    // best case
    String description = "Router description";
    String name = "router-name";
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription(description);
    routerArg.setName(name);
    ApiObjectId id = r.create(routerArg);
  }

  public void createQueue() {
    String description = "queue description";
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    q = new Queue(state);
    ApiObjectId id = q.create(queueArg);
  }

  @BeforeEach
  public void setup(){
      createRouter();
      createQueue();
  }


  @AfterEach
  public void cleanup() {
    //a.delete();
    //q.delete();
    //r.delete();
  }

  @Test
  @DisplayName("Create new agent.")
  public void createAgent() {
    // best case
    CreateAgentArg arg = new CreateAgentArg();
    ApiObjectId id = a.create(arg);
    AgentDto resource = a.get();
    assertThat(resource.getCapabilities(), nullValue());
    assertThat(String.format("Check state (%s) to be offline.", resource.getState()),
               resource.getState(), is(AgentState.offline));

  }

  @Test
  @DisplayName("Create new agent and bind to queue.")
  public void createAgentWithCapabilities() {
    a.create("en");
    AgentDto resource = a.get();
    assertThat(String.format("Check attribute language (%s) is 'en'.",
                             ((StringAttributeValueDto)resource.getCapabilities().get("language")).getValue()),
               ((StringAttributeValueDto)resource.getCapabilities().get("language")).getValue(), is("en"));
    assertThat(String.format("Check state (%s) to be offline.", resource.getState()),
               resource.getState(), is(AgentState.offline));

  }

  public void completeTask() throws MalformedURLException, InterruptedException {
    TimeUnit.SECONDS.sleep(2);
    AgentDto resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));
    TaskDto task = t.get();
    assertThat(String.format("Check task state (%s) to be assigned.", task.getState()),
               task.getState(), is(TaskState.assigned));

    t.setState(TaskState.completed);

    TimeUnit.SECONDS.sleep(2);

    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be ready.", resource.getState()),
               resource.getState(), is(AgentState.ready));

  }

  @Test
  @DisplayName("Create new agent and complete a task.")
  public void agentHandlesTask() throws MalformedURLException, InterruptedException {
    ApiObjectId id = a.create("en");
    AgentDto resource = a.get();
    assertThat(String.format("Check attribute language (%s) is 'en'.",
                             ((StringAttributeValueDto)resource.getCapabilities().get("language")).getValue()),
               ((StringAttributeValueDto)resource.getCapabilities().get("language")).getValue(), is("en"));
    assertThat(String.format("Check state (%s) to be offline.", resource.getState()),
               resource.getState(), is(AgentState.offline));
    assertThat(q.size(), is(0));
    a.setState(AgentState.ready);

    t.createQueueTask();
    assertThat(q.size(), is(0));
    completeTask();
    t.delete();
    a.setState(AgentState.offline);

  }

  @Test
  @DisplayName("Create new agent and complete two tasks.")
  public void agentHandlesTwoTasks() throws MalformedURLException, InterruptedException {
    ApiObjectId id = a.create("en");
    assertThat(q.size(), is(0));
    a.setState(AgentState.ready);
    t.createQueueTask();
    assertThat(q.size(), is(0));
    completeTask();
    t.delete();
    t.createQueueTask();
    assertThat(q.size(), is(0));
    completeTask();
    t.delete();
  }

  @Test
  @DisplayName("Create new agent task and set agent ready to complete task.")
  public void agentOfflineTaskReady() throws MalformedURLException, InterruptedException {
    ApiObjectId id = a.create("en");
    assertThat(q.size(), is(0));

    t.createQueueTask();

    assertThat(q.size(), is(1));

    a.setState(AgentState.ready);

    TimeUnit.SECONDS.sleep(2);
    AgentDto resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));
    TaskDto task = t.get();
    assertThat(String.format("Check task state (%s) to be assigned.", task.getState()),
               task.getState(), is(TaskState.assigned));

    t.setState(TaskState.completed);

    TimeUnit.SECONDS.sleep(2);

    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be ready.", resource.getState()),
               resource.getState(), is(AgentState.ready));

    t.delete();

  }

  @Test
  @DisplayName("Create new agent with 404 callback - agent should be unavailable.")
  public void taskWith404Callback() throws MalformedURLException, InterruptedException {
    ApiObjectId id = a.create("en");
    assertThat(q.size(), is(0));

    t.createQueueTask();

    assertThat(q.size(), is(1));

    a.setState(AgentState.ready);

    TimeUnit.SECONDS.sleep(2);
    AgentDto resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));
    TaskDto task = t.get();
    assertThat(String.format("Check task state (%s) to be assigned.", task.getState()),
               task.getState(), is(TaskState.assigned));
    t.setState(TaskState.completed);

    assertThat(q.size(), is(0));

    TimeUnit.SECONDS.sleep(2);

    a.setState(AgentState.ready);

    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be ready after it was completed.", resource.getState()),
               resource.getState(), is(AgentState.ready));

    t.delete();

  }

  @Test
  @DisplayName("Create new agent reject task.")
  public void taskRejected() throws MalformedURLException, InterruptedException {
    a.create("en");
    assertThat(q.size(), is(0));

    t.createQueueTask(new URL("http://not-existing-google.com/not-found"));
    assertThat(q.size(), is(1));

    a.setState(AgentState.ready);
    TimeUnit.SECONDS.sleep(2);

    AgentDto resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));

    t.setState(TaskState.waiting);
    TaskDto task = t.get();
    assertThat(String.format("Check task state (%s) to be waiting.", task.getState()),
               task.getState(), is(TaskState.waiting));

    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be unavailable.", resource.getState()),
               resource.getState(), is(AgentState.unavailable));

    t.setState(TaskState.completed);

    assertThat(q.size(), is(0));

    TimeUnit.SECONDS.sleep(2);

    a.setState(AgentState.ready);

    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be ready after it was unavailable.", resource.getState()),
               resource.getState(), is(AgentState.ready));

    t.delete();
  }

  @Test
  @DisplayName("Multiple agents compete for a task.")
  public void multipleAgentsPerTask() throws MalformedURLException, InterruptedException {

    ApiObjectId id1 = a.create("en");
    assertThat(q.size(), is(0));
    a.setState(AgentState.ready);

    ApiObjectId id2 = a.create("en");
    assertThat(q.size(), is(0));
    a.setState(AgentState.ready);

    t.createQueueTask();
    TimeUnit.SECONDS.sleep(2);
    assertThat(q.size(), is(0));

    state.put(CommsRouterResource.AGENT, id1.getId());
    AgentDto resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));

    t.setState(TaskState.completed);

    assertThat(q.size(), is(0));

    TimeUnit.SECONDS.sleep(2);

    a.setState(AgentState.ready);

    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be ready after it was unavailable.", resource.getState()),
               resource.getState(), is(AgentState.ready));

    t.delete();
  }

  @Test
  @DisplayName("Multiple agents cancel task and go to the next.")
  public void multipleAgentsAndCancelTask() throws MalformedURLException, InterruptedException {

    ApiObjectId id1 = a.create("en");
    assertThat(q.size(), is(0));
    a.setState(AgentState.ready);

    ApiObjectId id2 = a.create("en");
    assertThat(q.size(), is(0));
    a.setState(AgentState.ready);

    t.createQueueTask();
    TimeUnit.SECONDS.sleep(2);
    assertThat(q.size(), is(0));

    state.put(CommsRouterResource.AGENT, id1.getId());
    AgentDto resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));

    t.setState(TaskState.waiting);
    assertThat(q.size(), is(0));

    TimeUnit.SECONDS.sleep(2);
    state.put(CommsRouterResource.AGENT, id2.getId());

    resource = a.get();
    assertThat(String.format("Check that next agent takes care of the task and state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));

    t.delete();
  }

  @Test
  @DisplayName("Multiple agents last busy starts a task.")
  public void multipleAgentsLastBusyStartsTask() throws MalformedURLException, InterruptedException {

    ApiObjectId a1_id = a.create("en");
    assertThat(q.size(), is(0));
    a.setState(AgentState.ready);
    CreatedTaskDto task1 = t.createQueueTask();
    AgentDto resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));



    ApiObjectId a2_id = a.create("en");
    assertThat(q.size(), is(0));
    a.setState(AgentState.ready);
    assertThat(q.size(), is(0));

    CreatedTaskDto task2 = t.createQueueTask();
    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));
    t.setState(TaskState.completed);
    TimeUnit.SECONDS.sleep(2);// in order to ensure enough time granularity

    state.put(CommsRouterResource.TASK, task1.getId());
    t.setState(TaskState.completed);
    TimeUnit.SECONDS.sleep(2);// in order to ensure enough time granularity

    t.createQueueTask();
    TimeUnit.SECONDS.sleep(2);
    assertThat(q.size(), is(0));
    TaskDto task = t.get();
    assertThat(String.format("Check task state (%s) to be assigned.", task.getState()),
               task.getState(), is(TaskState.assigned));
    assertThat(String.format("Check task is assigned to the latest (%s) agent.", task.getAgentId()),
               task.getAgentId(), is(a1_id.getId()));

    t.delete();
  }

  @Test
  @DisplayName("Two tasks in a row.")
  public void twoTaskInARow() throws MalformedURLException, InterruptedException {

    ApiObjectId a1_id = a.create("en");
    assertThat(q.size(), is(0));
    CreatedTaskDto task1 = t.createQueueTask();
    TimeUnit.SECONDS.sleep(2);
    CreatedTaskDto task2 = t.createQueueTask();
    assertThat(q.size(), is(2));
    a.setState(AgentState.ready);
    TimeUnit.SECONDS.sleep(2);

    assertThat(q.size(), is(1));

    AgentDto resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));

    state.put(CommsRouterResource.TASK, task1.getId());
    TaskDto task = t.get();
    assertThat(String.format("Check task state (%s) to be assigned.", task.getState()),
               task.getState(), is(TaskState.assigned));
    t.setState(TaskState.completed);
    TimeUnit.SECONDS.sleep(2);

    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy with the second task.", resource.getState()),
               resource.getState(), is(AgentState.busy));

    state.put(CommsRouterResource.TASK, task2.getId());
    task = t.get();
    assertThat(String.format("Check task state (%s) to be assigned.", task.getState()),
               task.getState(), is(TaskState.assigned));
    t.setState(TaskState.completed);
    TimeUnit.SECONDS.sleep(2);

    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be ready when all tasks are completed.", resource.getState()),
               resource.getState(), is(AgentState.ready));
  }

}
