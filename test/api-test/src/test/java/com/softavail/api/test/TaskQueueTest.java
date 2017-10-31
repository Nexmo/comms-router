package com.softavail.api.test;

import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import java.util.HashMap;
import java.net.URL;
import java.net.MalformedURLException;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import java.util.Collections;

/**
 * Unit test for Task to queue mapping.
 */

@DisplayName("Task to Queue mapping Tests")
public class TaskQueueTest {

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Queue q = new Queue(state);
  private Plan p = new Plan(state);
  private Task t = new Task(state);

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
    id = q.create(queueArg);
  }

  @AfterEach
  public void cleanup() {
    t.delete();
    p.delete();
    q.delete();
    r.delete();
  }

  private void createPlan(String predicate) {
    CreatePlanArg arg = new CreatePlanArg();
    arg.setDescription("Rule with predicate " + predicate);
    RuleDto rule = new RuleDto();
    rule.setPredicate(predicate);
    RouteDto route = new RouteDto();
    route.setQueueId(state.get(CommsRouterResource.QUEUE));
    rule.getRoutes().add(route);
    arg.setRules(Collections.singletonList(rule));
    arg.setDefaultRoute(route);
    ApiObjectId id = p.create(arg);
  }

  private void createTask(AttributeGroupDto requirements) throws MalformedURLException {
    CreateTaskArg arg = new CreateTaskArg();
    arg.setCallbackUrl(new URL("http://example.com"));
    arg.setRequirements(requirements);
    t.createWithPlan(arg);
  }

  private void addPlanTask(AttributeGroupDto requirements, String predicate)
      throws MalformedURLException {
    createPlan(predicate);
    createTask(requirements);
  }

  @Test
  @DisplayName("Add task with no attribs queue.")
  public void addTask() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    addPlanTask(taskAttribs, "1==1");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute to queue.")
  public void addTaskOneAttribute() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("lang", new StringAttributeValueDto("en"));
    addPlanTask(taskAttribs, "1==1");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate to check it - ==.")
  public void addTaskOneAttributeEquals() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("lang", new StringAttributeValueDto("en"));
    addPlanTask(taskAttribs, "#{lang}=='en'");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with float attribute and predicate to check it - ==.")
  public void addTaskFloatAttributeEquals() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("float", new DoubleAttributeValueDto(0.05));
    addPlanTask(taskAttribs, "#{float}==0.05");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate to check it - !=.")
  public void addTaskOneAttributeNotEquals() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("lang", new StringAttributeValueDto("en"));
    addPlanTask(taskAttribs, "#{lang}!='bg'");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate to check it - number >.")
  public void addTaskOneAttributeCompare() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "#{age}>18 && #{age}<33");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate to check it - with parents.")
  public void addTaskOneAttributeParents() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "(#{age}>18 && #{age}<33)");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate to check it - or.")
  public void addTaskOneAttributeOr() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "1 || 0");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate to check it 1 && 1")
  public void addTaskOneAttributeAndOnly() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "1 && 1");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate to check it true && true")
  public void addTaskTrue() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "true && true");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate HAS")
  public void addTaskHasExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "HAS([10,20,30],#{age})");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate HAS with single item")
  public void addTaskHasOneItemExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "HAS([10],#{age})");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate HAS with no items")
  public void addTaskHasNoItemExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "HAS([],#{age})");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate HAS that should fail.")
  public void addTaskHasFailed() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "HAS([9],#{age})");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate IN")
  public void addTaskExpressionIn() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "IN(#{age},[10,20,30])");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate to check it false || true || false")
  public void addTaskTrueFalseExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "false || true || false");
    assertThat(q.size(), is(1));
  }

}
