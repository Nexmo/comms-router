/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
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

  // Testing the create method
  @Test
  public void createTest() throws CommsRouterException {
    RouterObjectId id = new RouterObjectId("", "01");
    agentService.create(newCreateAgentArg("address_one"), id);
    queueService.create(newCreateQueueArg("1==1", "description_one"), id);
    QueueDto queue = queueService.get(id);
    assertEquals(queue.getDescription(), "description_one");
  }

  // Testing the update method
  @Test
  public void updateTest() throws CommsRouterException {
    RouterObjectId id = new RouterObjectId("", "01");
    agentService.create(newCreateAgentArg("address_one"), id);
    queueService.create(newCreateQueueArg("1==1", "description_one"), id);
    QueueDto queueBefore = queueService.get(id);
    // Updating
    queueService.update(newUpdateQueueArg("1==1", "description_two"), id);
    QueueDto queueAfter = queueService.get(id);
    assertNotEquals(queueAfter, queueBefore);
  }

  // Testing the getQueueSize method
  @Test
  public void getQueueSizeTest() throws CommsRouterException, MalformedURLException {
    RouterObjectId id = new RouterObjectId("", "01");
    ApiObjectId queue = queueService.create(newCreateQueueArg("1==1", "description_one"), id);
    taskService.create(newCreateTaskArg(id.getId(), "https://test.com", null), id);
    RouterObjectId idd = new RouterObjectId(queue.getId(), "01");
    long size = queueService.getQueueSize(idd);
    assertEquals(size, 1);
  }

  // Testing the getTasks method
  @Test
  public void getTasksTest() throws CommsRouterException, MalformedURLException {
    RouterObjectId id = new RouterObjectId("", "01");
    ApiObjectId queue = queueService.create(newCreateQueueArg("1==1", "description_one"), id);
    taskService.create(newCreateTaskArg(id.getId(), "https://test.com", null), id);
    RouterObjectId idd = new RouterObjectId(queue.getId(), "01");
    Collection<TaskDto> tasks = queueService.getTasks(idd);
    assertEquals(tasks.size(), 1);
    assertEquals(tasks.iterator().next().getCallbackUrl(), "https://test.com");
  }

}
