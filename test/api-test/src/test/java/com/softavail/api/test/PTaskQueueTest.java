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
import java.util.Arrays;
import java.util.concurrent.TimeUnit;

/**
 * Unit test for Task to queue mapping.
 */

@DisplayName("Task to Queue mapping Tests")
public class PTaskQueueTest {

  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Queue q = new Queue(state);
  private Plan p = new Plan(state);
  private Task t = new Task(state);
  private String defaultQueueId;
  private String backupQueueId;
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
    defaultQueueId = q.create(new CreateQueueArg.Builder()
                              .predicate(predicate)
                              .description("queue description").build())
        .getId();

    backupQueueId = q.create(new CreateQueueArg.Builder()
                              .predicate(predicate)
                              .description("backup queue description").build())
        .getId();

    id = q.create(new CreateQueueArg.Builder()
                  .predicate(predicate)
                  .description("queue description").build());
  }

    //@AfterEach
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
    route.setTimeout(1L);
    RouteDto backupRoute = new RouteDto();
    backupRoute.setQueueId(backupQueueId);

    rule.setRoutes(Arrays.asList(route,backupRoute));
    arg.setRules(Collections.singletonList(rule));

    RouteDto defaultRoute = new RouteDto();
    defaultRoute.setQueueId(defaultQueueId);

    arg.setDefaultRoute(defaultRoute);
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
  @DisplayName("Add task with one attribute to queue.")
  public void addTaskOneAttribute() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("lang", new StringAttributeValueDto("en"));
    addPlanTask(taskAttribs, "1==1");
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate HAS with single item")
  public void addTaskHasOneItemExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "HAS([10],#{age})");
    assertThat(q.size(), is(0));
    state.put(CommsRouterResource.QUEUE,defaultQueueId);
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with one attribute and predicate HAS with no items")
  public void addTaskHasNoItemExpression() throws MalformedURLException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "HAS([],#{age})");
    assertThat(q.size(), is(0));
    state.put(CommsRouterResource.QUEUE,defaultQueueId);
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with timed out queue")
  public void addTaskTimedoutQueue() throws MalformedURLException, InterruptedException {
    assertThat(q.size(), is(0));
    AttributeGroupDto taskAttribs = new AttributeGroupDto();
    taskAttribs.put("age", new DoubleAttributeValueDto(20));
    addPlanTask(taskAttribs, "HAS([20],#{age})");
    assertThat(q.size(), is(1));
    TimeUnit.SECONDS.sleep(2);
    assertThat(String.format("Router %s. Check task is not in the queue after the timeout.", state.get(CommsRouterResource.ROUTER)),
               q.size(), is(0));
    state.put(CommsRouterResource.QUEUE,backupQueueId);
    assertThat(q.size(), is(1));
  }

}
