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
import com.softavail.commsrouter.test.api.Agent;
import com.softavail.commsrouter.test.api.Task;
import com.softavail.commsrouter.test.api.Router;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.equalTo;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.concurrent.TimeUnit;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import org.junit.*;

public class PlanTest extends BaseTest {

    /**
     * Creates a new <code>PlanTest</code> instance.
     *
     */

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Queue q = new Queue(state);
  private Plan p = new Plan(state);
  private Task t = new Task(state);
  private String defaultQueueId;

  @Before
  public void createRouterAndQueue() {
    CreateRouterArg routerArg = new CreateRouterArg();
    routerArg.setDescription("Router description");
    routerArg.setName("router-name");
    ApiObjectRef ref = r.create(routerArg);

    String predicate = "1==1";
    CreateQueueArg queueArg = new CreateQueueArg();
    queueArg.setDescription("queue description");
    queueArg.setPredicate(predicate);
    q = new Queue(state);
    defaultQueueId = q.create(new CreateQueueArg.Builder()
                              .predicate(predicate)
                              .description("queue description").build())
        .getRef();
  }

  @Test
  //@DisplayName("Delete plan when it is in use.")
  public void deletePlanInUse() throws MalformedURLException {
    assertThat(q.size(), is(0));
    String predicate = "1 == 1";
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                          new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                          new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());
    t.createWithPlan(new CreateTaskArg.Builder()
                     .callback(new URL("http://localhost:8080"))
                     .requirements(new AttributeGroupDto()
                                   .withKeyValue("lang", new StringAttributeValueDto("en")))
                                   .build());
    p.deleteResponse()
        .statusCode(500)
        .body("error.description",
              equalTo("Cannot delete or update 'route' as there is record in 'task' that refer to it."));
  }

  @Test
  //@DisplayName("Update plan when it is in use.")
  public void updatePlanInUse() throws MalformedURLException {
    assertThat(q.size(), is(0));
    String predicate = "1 == 1";
    p.create(new CreatePlanArg.Builder("Rule with predicate " + predicate)
             .rules(Collections.singletonList(new RuleDto.Builder(predicate)
                                              .routes(Arrays.asList(
                                                          new RouteDto.Builder(defaultQueueId).timeout(1L).build(),
                                                          new RouteDto.Builder(defaultQueueId).build()))
                                              .build()))
             .defaultRoute(new RouteDto.Builder(defaultQueueId).build())
             .build());
    t.createWithPlan(new CreateTaskArg.Builder()
                     .callback(new URL("http://localhost:8080"))
                     .requirements(new AttributeGroupDto()
                                   .withKeyValue("lang", new StringAttributeValueDto("en")))
                                   .build());
    UpdatePlanArg updateArgs = new UpdatePlanArg();
    updateArgs.setDescription("updated description");
    p.update(updateArgs);
  }
}
