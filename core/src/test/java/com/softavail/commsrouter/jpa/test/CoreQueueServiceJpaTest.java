/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import java.net.MalformedURLException;
import java.util.Collection;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CoreQueueServiceJpaTest extends TestBase {

  // Testing the replace method
  @Test
  public void createTest() throws CommsRouterException {
    RouterObjectRef id = new RouterObjectRef("", "01");
    agentService.replace(newCreateAgentArg("address_one"), id);
    queueService.replace(newCreateQueueArg("1==1", "description_one"), id);
    QueueDto queue = queueService.get(id);
    assertEquals(queue.getDescription(), "description_one");
  }

  // Testing the update method
  @Test
  public void updateTest() throws CommsRouterException {
    RouterObjectRef id = new RouterObjectRef("", "01");
    agentService.replace(newCreateAgentArg("address_one"), id);
    queueService.replace(newCreateQueueArg("1==1", "description_one"), id);
    QueueDto queueBefore = queueService.get(id);
    // Updating
    queueService.update(newUpdateQueueArg("1==1", "description_two"), queueBefore);
    QueueDto queueAfter = queueService.get(id);
    assertNotEquals(queueAfter, queueBefore);
  }

  // Testing the getQueueSize method
  @Test
  public void getQueueSizeTest() throws CommsRouterException, MalformedURLException {
    RouterObjectRef ref = new RouterObjectRef("", "01");
    ApiObjectRef queue = queueService.replace(newCreateQueueArg("1==1", "description_one"), ref);
    taskService.replace(newCreateTaskArg(ref.getRef(), "https://test.com", null), ref);
    RouterObjectRef idd = new RouterObjectRef(queue.getRef(), "01");
    long size = queueService.getQueueSize(idd);
    assertEquals(size, 1);
  }

  // Testing the getTasks method
  @Test
  public void getTasksTest() throws CommsRouterException, MalformedURLException {
    RouterObjectRef ref = new RouterObjectRef("", "01");
    ApiObjectRef queue = queueService.replace(newCreateQueueArg("1==1", "description_one"), ref);
    taskService.replace(newCreateTaskArg(ref.getRef(), "https://test.com", null), ref);
    RouterObjectRef idd = new RouterObjectRef(queue.getRef(), "01");
    Collection<TaskDto> tasks = queueService.getTasks(idd);
    assertEquals(tasks.size(), 1);
    assertEquals(tasks.iterator().next().getCallbackUrl(), "https://test.com");
  }

}
