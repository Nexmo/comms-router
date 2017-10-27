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

@DisplayName("Task related tests")
public class TaskTest {

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
    q.delete();
    r.delete();
  }

  @Test
  @DisplayName("Add task with queue.")
  public void addTask() throws MalformedURLException {
    assertThat(q.size(), is(0));
    CreateTaskArg arg = new CreateTaskArg();
    arg.setCallbackUrl(new URL("http://example.com"));
    arg.setRequirements(new AttributeGroupDto()
                        .withKeyValue("language", new StringAttributeValueDto ("en")));
    arg.setQueueId(state.get(CommsRouterResource.QUEUE));
    t.createWithPlan(arg);
    assertThat(q.size(), is(1));
  }

  @Test
  @DisplayName("Add task with context.")
  public void addTaskWithContext() throws MalformedURLException {
    assertThat(q.size(), is(0));
    CreateTaskArg arg = new CreateTaskArg();
    arg.setCallbackUrl(new URL("http://example.com"));
    arg.setRequirements(new AttributeGroupDto());
    arg.setUserContext(new AttributeGroupDto().withKeyValue("key", new StringAttributeValueDto ("Value")));

    arg.setQueueId(state.get(CommsRouterResource.QUEUE));
    t.create(arg);
    assertThat(q.size(), is(1));
  }

}
