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

import com.softavail.commsrouter.api.dto.arg.*;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.BooleanAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.skill.EnumerationAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberInterval;
import com.softavail.commsrouter.api.dto.model.skill.NumberIntervalBoundary;
import com.softavail.commsrouter.test.api.*;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.equalTo;

import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.QueueDto;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Set;
import java.util.List;
import org.junit.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Unit test for simple App.
 */
public class QueueSkillValidationTest extends BaseTest{

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Queue q = new Queue(state);
  private Skill s = new Skill(state);

  @Before
  public void createRouter() {
    String description = "Router description";
    String name = "router-name";
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription(description);
    routerArg.setName(name);
    ApiObjectRef ref = r.create(routerArg);
  }
  
  @After
  public void deleteRouter() {
    s.delete();
    r.delete();
  }

  @Test
  public void createQueueNoSkill() {
    String description = "queue description";
    String predicate = "missing==1";
    s = new Skill(state);
    Set<String> options = Stream.of("en","es").collect(Collectors.toSet());
    s.replace("language",
            new CreateSkillArg.Builder()
                    .name("language")
                    .description("domain")
                    .domain( new EnumerationAttributeDomainDto(options))
                    .multivalue(false)
                    .build());

    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    ApiQueue api_q = new ApiQueue(state);

    api_q.create(state.get(CommsRouterResource.ROUTER), queueArg)
            .statusCode(400).body("error.description",
            is("Skill missing was not found."));
  }

  @Test
  public void createQueueSkillInvalidValue() {
    String description = "queue description";
    String predicate = "language==1";

    s = new Skill(state);
    Set<String> options = Stream.of("en","es").collect(Collectors.toSet());
    s.replace("language",
            new CreateSkillArg.Builder()
                    .name("language")
                    .description("domain")
                    .domain( new EnumerationAttributeDomainDto(options))
                    .multivalue(false)
                    .build());

    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);
    ApiQueue api_q = new ApiQueue(state);

    api_q.create(state.get(CommsRouterResource.ROUTER), queueArg)
            .statusCode(400).body("error.description",
            is("1' is not a valid value for skill language."));
  }

  @Test
  public void createQueueSkillOutOfRangeValue() {
    String description = "queue description";
    String predicate = "num==110";

    s = new Skill(state);
    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
            new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
            new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(50.0,true))
    ).collect(Collectors.toList());
    s.replace("num",
            new CreateSkillArg.Builder()
                    .name("num")
                    .description("domain")
                    .domain( new NumberAttributeDomainDto(intervals))
                    .multivalue(false)
                    .build());

    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription(description);
    queueArg.setPredicate(predicate);

    q.create(queueArg);
    q.delete();
    s.delete();
  }

}
