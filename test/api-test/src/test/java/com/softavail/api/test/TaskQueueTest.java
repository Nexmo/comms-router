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

import com.softavail.commsrouter.test.api.CommsRouterResource;
import com.softavail.commsrouter.test.api.Queue;
import com.softavail.commsrouter.test.api.Plan;
import com.softavail.commsrouter.test.api.Skill;
import com.softavail.commsrouter.test.api.Task;
import com.softavail.commsrouter.test.api.Router;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.*;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.CreateSkillArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.*;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.skill.*;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.Set;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;


/**
 * Unit test for Task to queue mapping.
 */

//@DisplayName("Task to Queue mapping Tests")
public class TaskQueueTest extends BaseTest {

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Queue q = new Queue(state);
  private Plan p = new Plan(state);
  private Skill s = new Skill(state);
  private Task t = new Task(state);
  private String defaultQ;

  @Before
  public void createRouterAndQueue() {
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription("Router description");
    routerArg.setName("router-name");
    ApiObjectRef ref = r.create(routerArg);

    Set<String> options = Stream.of("en","es","bg").collect(Collectors.toSet());
    s.replace("lang", new CreateSkillArg.Builder()
             .name("language")
             .description("language domain")
             .domain( new EnumerationAttributeDomainDto(options))
             .multivalue(false)
             .build());

    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
                                               new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
                                               new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(50.0,true))
                                               ).collect(Collectors.toList());
    s.replace("age", new CreateSkillArg.Builder()
             .name("age")
             .description("age domain")
             .domain( new NumberAttributeDomainDto(intervals))
             .multivalue(false)
             .build());
    
    s.replace("float", new CreateSkillArg.Builder()
              .name("float")
              .description("age domain")
              .domain( new NumberAttributeDomainDto(intervals))
              .multivalue(false)
              .build());
    
    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setPredicate(predicate);
    q = new Queue(state);
    queueArg.setPredicate(predicate);
    queueArg.setDescription("default queue");
    defaultQ = q.create(queueArg).getRef();

    queueArg.setPredicate(predicate);
    queueArg.setDescription("queue description");
    q.create(queueArg);
  }

  @After
  public void cleanup() {
    t.delete();
    p.delete();
    q.delete();
    state.put(CommsRouterResource.QUEUE,defaultQ);
    q.delete();
    assertThat(s.list().stream().map((SkillDto dto)-> { s.delete(dto.getRef());return dto;}).count()
               , is(3L));
    r.delete();
  }

  private void createPlan(String predicate) {
    CreatePlanArg arg = new CreatePlanArg();
    arg.setDescription("Rule with predicate " + predicate);
    RuleDto rule = new RuleDto();
    rule.setPredicate(predicate);
    RouteDto route = new RouteDto();
    route.setQueueRef(state.get(CommsRouterResource.QUEUE));
    rule.getRoutes().add(route);
    arg.setRules(Collections.singletonList(rule));

    RouteDto defaultRoute = new RouteDto();
    defaultRoute.setQueueRef(defaultQ);

    arg.setDefaultRoute(defaultRoute);

    ApiObjectRef ref = p.create(arg);
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

  @SuppressWarnings("unchecked")
  @Test
  public void addTask() throws MalformedURLException {
    q.checkSize(0);
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    addPlanTask(taskAttribs, "1==1");
    q.checkSize(1);
  }

  @Test
  public void addTaskDoNotMatch() throws MalformedURLException {
    q.checkSize(0);
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("lang", new StringAttributeValueDto("en"));
    addPlanTask(taskAttribs, "lang=out=(\"bg\",\"en\",\"es\")");
    q.checkSize(0);
  }

  @Test
  public void addTaskOneAttribute() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("lang", new StringAttributeValueDto("en"));
    addPlanTask(taskAttribs, "1==1");
    assertThat(q.size(), is(1));
  }

  @Test
  public void addTaskOneAttributeMatch() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("lang", new StringAttributeValueDto("en"));
    addPlanTask(taskAttribs, "lang==en");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it - ==.")
  public void addTaskOneAttributeEquals() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("lang", new StringAttributeValueDto("en"));
    addPlanTask(taskAttribs, "#{lang}=='en'");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it - ==.")
  public void addTaskOneAttributeEqualsRSQL() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("lang", new StringAttributeValueDto("en"));
    addPlanTask(taskAttribs, "lang=='en'");
    assertThat(q.size(), is(1));
  }

  @Test
  public void addTaskFloatAttributeEquals() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("float", new DoubleAttributeValueDto(1.05));
    addPlanTask(taskAttribs, "#{float}==1.05");
    assertThat(q.size(), is(1));
  }

  @Test
  public void addTaskFloatAttributeEqualsRSQL() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("float", new DoubleAttributeValueDto(1.05));
    addPlanTask(taskAttribs, "float==1.05");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it - !=.")
  public void addTaskOneAttributeNotEquals() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("lang", new StringAttributeValueDto("en"));
    addPlanTask(taskAttribs, "#{lang}!='bg'");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it - !=.")
  public void addTaskOneAttributeNotEqualsRSQL() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("lang", new StringAttributeValueDto("en"));
    addPlanTask(taskAttribs, "lang!='bg'");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it - number >.")
  public void addTaskOneAttributeCompare() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "#{age}>18 && #{age}<33");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it - number >.")
  public void addTaskOneAttributeCompareRSQL() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "age>18 and age<33");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it - with parents.")
  public void addTaskOneAttributeParents() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "(#{age}>18 && #{age}<33)");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it - with parents.")
  public void addTaskOneAttributeParentsRSQL() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "(age>18 and age<33)");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it - or.")
  public void addTaskOneAttributeOr() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "(HAS([10,20,30],#{age}) || HAS([10,20,30],#{age}))");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it - or.")
  public void addTaskOneAttributeOrRSQL() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "age>1 or age=lt=1");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it 1 && 1")
  public void addTaskOneAttributeAndOnly() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "HAS([10,20,30],#{age}) && HAS([10,20,30],#{age})");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate to check it 1 && 1")
  public void addTaskOneAttributeAndOnlyRSQL() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "age=in=(10,20,30) and age=out=(10,2,30)");
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate HAS with single item")
  public void addTaskHasOneItemExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "HAS([10],#{age})");
    assertThat(q.size(), is(0));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate HAS with single item")
  public void addTaskHasOneItemExpressionRSQL() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "age=in=(10)");
    assertThat(q.size(), is(0));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate HAS with single item")
  public void addTaskOutOneItemExpressionRSQL() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "age=out=(20)");
    assertThat(q.size(), is(0));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate HAS with no items")
  public void addTaskHasNoItemExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "HAS([],#{age})");
    assertThat(q.size(), is(0));
  }

  @Test
  //@DisplayName("Add task with one attribute and predicate IN")
  public void addTaskExpressionIn() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "IN(#{age},[10,20,30])");
    assertThat(q.size(), is(1));
  }

}
