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
import com.softavail.commsrouter.test.api.Skill;
import com.softavail.commsrouter.test.api.Task;
import com.softavail.commsrouter.test.api.Router;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.allOf;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateSkillArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
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
 * Unit test for Task to queue mapping.
 */

// @DisplayName("Task related tests")
public class TaskTest extends BaseTest{

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Queue q = new Queue(state);
  private Plan p = new Plan(state);
  private Skill s = new Skill(state);
  private Task t = new Task(state);


  @Before
  public void createRouterAndQueue() {
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription("Router description");
    routerArg.setName("router-name");
    ApiObjectRef ref = r.create(routerArg);
    s.replace("language", new CreateSkillArg.Builder()
              .name("language")
              .description("language domain")
              .domain( new EnumerationAttributeDomainDto(Stream.of("en","es").collect(Collectors.toSet())))
              .multivalue(false)
              .build());

    s.replace("age", new CreateSkillArg.Builder()
              .name("age")
              .description("age domain")
              .domain( new EnumerationAttributeDomainDto(Stream.of("en","es").collect(Collectors.toSet())))
              .multivalue(false)
              .build());

    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription("queue description");
    queueArg.setPredicate(predicate);
    q = new Queue(state);
    ref = q.create(queueArg);
  }

  @After
  public void cleanup() {
    t.delete();
    q.delete();
    assertThat(s.list().stream().map((SkillDto dto)-> { s.delete(dto.getRef());return dto;}).count()
               , is(2L));
    r.delete();
  }

  @Test
  //@DisplayName("Add task with queue.")
  public void addTask() throws MalformedURLException {
    assertThat(q.size(), is(0));
    CreateTaskArg arg = new CreateTaskArg();
    arg.setCallbackUrl(new URL("http://example.com"));
    arg.setRequirements(new AttributeGroupDto()
                        .withKeyValue("language", new StringAttributeValueDto("en")));
    arg.setQueueRef(state.get(CommsRouterResource.QUEUE));
    t.createWithPlan(arg);
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with context.")
  public void addTaskWithContext() throws MalformedURLException {
    assertThat(q.size(), is(0));
    CreateTaskArg arg = new CreateTaskArg();
    arg.setCallbackUrl(new URL("http://example.com"));
    arg.setRequirements(new AttributeGroupDto());
    arg.setUserContext(
        new AttributeGroupDto().withKeyValue("key", new StringAttributeValueDto("Value")));

    arg.setQueueRef(state.get(CommsRouterResource.QUEUE));
    t.create(arg);
    assertThat(q.size(), is(1));
  }

  @Test
  //@DisplayName("Add task with tag.")
  public void addTaskWithTag() throws MalformedURLException {
    assertThat(q.size(), is(0));
    CreateTaskArg arg = new CreateTaskArg();
    arg.setCallbackUrl(new URL("http://example.com"));
    arg.setRequirements(new AttributeGroupDto());
    arg.setUserContext(
        new AttributeGroupDto().withKeyValue("key", new StringAttributeValueDto("Value")));
    arg.setQueueRef(state.get(CommsRouterResource.QUEUE));
    String uniqueTag = state.get(CommsRouterResource.QUEUE);
    arg.setTag(uniqueTag);
    t.create(arg);
    assertThat(q.size(), is(1));
    assertThat(t.list("?tag="+uniqueTag).size(),is(1));
  }

  @Test
  //@DisplayName("Add task with existing tag.")
  public void addTaskWithExistingTag() throws MalformedURLException {
    assertThat(q.size(), is(0));
    CreateTaskArg arg = new CreateTaskArg();
    arg.setCallbackUrl(new URL("http://example.com"));
    arg.setRequirements(new AttributeGroupDto());
    arg.setUserContext(
        new AttributeGroupDto().withKeyValue("key", new StringAttributeValueDto("Value")));
    arg.setQueueRef(state.get(CommsRouterResource.QUEUE));
    String uniqueTag = state.get(CommsRouterResource.QUEUE);
    arg.setTag(uniqueTag);
    t.create(arg);
    assertThat(t.list("?tag="+uniqueTag).size(),is(1));
    t.createResponse(arg)
        .statusCode(500)
        .body("error.description",
              allOf(containsString("Duplicate entry"),
                    containsString("for key 'task_tag_index'")));
  }

}
