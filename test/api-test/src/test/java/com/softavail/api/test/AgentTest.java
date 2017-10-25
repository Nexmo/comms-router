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
    q.delete();
    r.delete();
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
    a.delete();
  }

  @Test
  @DisplayName("Create new agent and bind to queue.")
  public void createAgentWithCapabilities() {
    CreateAgentArg arg = new CreateAgentArg();
    arg.setAddress("phonenumber");
    arg.setCapabilities(new AttributeGroupDto().withKeyValue("language", new StringAttributeValueDto("en")));
    ApiObjectId id = a.create(arg);
    AgentDto resource = a.get();
    assertThat(String.format("Check attribute language (%s) is 'en'.",
                             ((StringAttributeValueDto)resource.getCapabilities().get("language")).getValue()),
               ((StringAttributeValueDto)resource.getCapabilities().get("language")).getValue(), is("en"));
    assertThat(String.format("Check state (%s) to be offline.", resource.getState()),
               resource.getState(), is(AgentState.offline));
    a.delete();
  }

  public void completeTask() throws MalformedURLException, InterruptedException {
    CreateTaskArg taskArg = new CreateTaskArg();
    taskArg.setCallbackUrl(new URL("http://example.com"));
    taskArg.setRequirements(new AttributeGroupDto());
    taskArg.setQueueId(state.get(CommsRouterResource.QUEUE));
    t.create(taskArg);

    assertThat(q.size(), is(0));

    TimeUnit.SECONDS.sleep(1);
    AgentDto resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));
    TaskDto task = t.get();
    assertThat(String.format("Check task state (%s) to be assigned.", task.getState()),
               task.getState(), is(TaskState.assigned));

    UpdateTaskArg updateTask = new UpdateTaskArg();
    updateTask.setState(TaskState.completed);
    t.update(updateTask);

    TimeUnit.SECONDS.sleep(1);

    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be ready.", resource.getState()),
               resource.getState(), is(AgentState.ready));
    t.delete();
  }

  @Test
  @DisplayName("Create new agent and complete a task.")
  public void agentHandlesTask() throws MalformedURLException, InterruptedException {
    CreateAgentArg arg = new CreateAgentArg();
    arg.setAddress("phonenumber");
    arg.setCapabilities(new AttributeGroupDto().withKeyValue("language", new StringAttributeValueDto("en")));
    ApiObjectId id = a.create(arg);
    AgentDto resource = a.get();
    assertThat(String.format("Check attribute language (%s) is 'en'.",
                             ((StringAttributeValueDto)resource.getCapabilities().get("language")).getValue()),
               ((StringAttributeValueDto)resource.getCapabilities().get("language")).getValue(), is("en"));
    assertThat(String.format("Check state (%s) to be offline.", resource.getState()),
               resource.getState(), is(AgentState.offline));
    assertThat(q.size(), is(0));
    a.setState(AgentState.ready);
    completeTask();
    a.setState(AgentState.offline);
    a.delete();
  }

  @Test
  @DisplayName("Create new agent and complete two tasks.")
  public void agentHandlesTwoTasks() throws MalformedURLException, InterruptedException {
    CreateAgentArg arg = new CreateAgentArg();
    arg.setAddress("phonenumber");
    arg.setCapabilities(new AttributeGroupDto().withKeyValue("language", new StringAttributeValueDto("en")));
    ApiObjectId id = a.create(arg);
    assertThat(q.size(), is(0));
    a.setState(AgentState.ready);
    completeTask();
    completeTask();
    a.delete();
  }

  @Test
  @DisplayName("Create new agent task and set agent ready to complete task.")
  public void agentOfflineTaskReady() throws MalformedURLException, InterruptedException {
    CreateAgentArg arg = new CreateAgentArg();
    arg.setAddress("phonenumber");
    arg.setCapabilities(new AttributeGroupDto().withKeyValue("language", new StringAttributeValueDto("en")));
    ApiObjectId id = a.create(arg);
    assertThat(q.size(), is(0));

    CreateTaskArg taskArg = new CreateTaskArg();
    taskArg.setCallbackUrl(new URL("http://example.com"));
    taskArg.setRequirements(new AttributeGroupDto());
    taskArg.setQueueId(state.get(CommsRouterResource.QUEUE));
    t.create(taskArg);

    assertThat(q.size(), is(1));

    a.setState(AgentState.ready);

    TimeUnit.SECONDS.sleep(1);
    AgentDto resource = a.get();
    assertThat(String.format("Check agent state (%s) to be busy.", resource.getState()),
               resource.getState(), is(AgentState.busy));
    TaskDto task = t.get();
    assertThat(String.format("Check task state (%s) to be assigned.", task.getState()),
               task.getState(), is(TaskState.assigned));

    UpdateTaskArg updateTask = new UpdateTaskArg();
    updateTask.setState(TaskState.completed);
    t.update(updateTask);

    TimeUnit.SECONDS.sleep(1);

    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be ready.", resource.getState()),
               resource.getState(), is(AgentState.ready));

    t.delete();
    a.delete();
  }

  @Test
  @DisplayName("Create new agent with 404 callback - agent should be unavailable.")
  public void taskWith404Callback() throws MalformedURLException, InterruptedException {
    CreateAgentArg arg = new CreateAgentArg();
    arg.setAddress("phonenumber");
    arg.setCapabilities(new AttributeGroupDto().withKeyValue("language", new StringAttributeValueDto("en")));
    ApiObjectId id = a.create(arg);
    assertThat(q.size(), is(0));

    CreateTaskArg taskArg = new CreateTaskArg();
    taskArg.setCallbackUrl(new URL("http://google.com/not-found"));
    taskArg.setRequirements(new AttributeGroupDto());
    taskArg.setQueueId(state.get(CommsRouterResource.QUEUE));
    t.create(taskArg);

    assertThat(q.size(), is(1));

    a.setState(AgentState.ready);

    TimeUnit.SECONDS.sleep(1);
    AgentDto resource = a.get();
    assertThat(String.format("Check agent state (%s) to be unavailable.", resource.getState()),
               resource.getState(), is(AgentState.unavailable));
    TaskDto task = t.get();
    assertThat(String.format("Check task state (%s) to be waiting.", task.getState()),
               task.getState(), is(TaskState.waiting));

    UpdateTaskArg updateTask = new UpdateTaskArg();
    updateTask.setState(TaskState.completed);
    t.update(updateTask);

    assertThat(q.size(), is(0));

    TimeUnit.SECONDS.sleep(1);

    a.setState(AgentState.ready);

    resource = a.get();
    assertThat(String.format("Check agent state (%s) to be ready after it was unavailable.", resource.getState()),
               resource.getState(), is(AgentState.ready));

    t.delete();
    a.delete();
  }


}
