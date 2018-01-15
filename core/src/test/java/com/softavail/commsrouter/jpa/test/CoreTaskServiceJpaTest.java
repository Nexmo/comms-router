/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import org.junit.Test;

import java.net.MalformedURLException;

/**
 * @author G.Ivanov
 */
public class CoreTaskServiceJpaTest extends TestBase {

  // Testing the replace method that takes a RouterObjectRef
  @Test
  public void createTest() throws MalformedURLException, CommsRouterException {
    RouterObjectRef ref = new RouterObjectRef("", "01");
    ApiObjectRef queue = queueService.replace(newCreateQueueArg("1==1", "desctiption_one"), ref);
    taskService.replace(newCreateTaskArg(queue.getRef(), "https://test.com", null), ref);
    TaskDto task = taskService.get(ref);
    assertEquals(task.getCallbackUrl(), "https://test.com");
  }

  // Testing the replace method that takes a String routerId
  @Test
  public void createTestTwo() throws MalformedURLException, CommsRouterException {
    ApiObjectRef queue = queueService.create(newCreateQueueArg("1==1", "desctiption_one"), "01");
    taskService.create(newCreateTaskArg(queue.getRef(), "https://test.com", null), "01");
    PaginatedList<TaskDto> task = taskService.list(
        new PagingRequest("01", null, 10, null, null));
    assertEquals(task.getList().get(0).getCallbackUrl(), "https://test.com");
  }

  @Test
  public void cancelTest() throws CommsRouterException, MalformedURLException {
    RouterObjectRef ref = new RouterObjectRef("", "01");
    ApiObjectRef queue = queueService.replace(newCreateQueueArg("1==1", "desctiption_one"), ref);
    taskService.replace(newCreateTaskArg(queue.getRef(), "https://test_one.com", null), ref);
    // Updating
    taskService.update(newUpdateTaskArg(2, TaskState.canceled), ref);
    TaskDto task = taskService.get(ref);
    assertEquals(task.getState(), TaskState.canceled);
  }

  // Testing the update method that updates the TaskContext
  @Test
  public void updateContextTest_one() throws CommsRouterException, MalformedURLException {
    RouterObjectRef ref = new RouterObjectRef("", "01");
    ApiObjectRef queue = queueService.replace(newCreateQueueArg("1==1", "desctiption_one"), ref);
    taskService.replace(newCreateTaskArg(queue.getRef(), "https://test_one.com", null), ref);
    UpdateTaskContext ctx = newUpdateTaskContext();
    TaskDto taskBefore = taskService.get(ref);
    // Updating
    taskService.update(ctx, ref);
    TaskDto taskAfter = taskService.get(ref);
    assertNotEquals(taskAfter.getUserContext(), taskBefore.getUserContext());
  }

  // Testing the updateContext method
  @Test
  public void updateContextTest_two() throws CommsRouterException, MalformedURLException {
    RouterObjectRef ref = new RouterObjectRef("", "01");
    ApiObjectRef queue = queueService.replace(newCreateQueueArg("1==1", "desctiption_one"), ref);
    taskService.replace(newCreateTaskArg(queue.getRef(), "https://test_one.com", null), ref);
    UpdateTaskContext ctx = newUpdateTaskContext();
    TaskDto taskBefore = taskService.get(ref);
    // Updating
    taskService.updateContext(ctx, ref);
    TaskDto taskAfter = taskService.get(ref);
    assertNotEquals(taskAfter.getUserContext(), taskBefore.getUserContext());
  }

  // Passing a state != to completed should throw a BadValueException
  @Test(expected = BadValueException.class)
  public void exceptionTestTwo() throws MalformedURLException, CommsRouterException {
    RouterObjectRef ref = new RouterObjectRef("", "01");
    ApiObjectRef queue = queueService.replace(newCreateQueueArg("1==1", "desctiption_one"), ref);
    taskService.replace(newCreateTaskArg(queue.getRef(), "https://test_one.com", null), ref);
    taskService.update(newUpdateTaskArg(2, TaskState.assigned), ref);
  }

  // Creating from a plan
  public void createFromPlan() throws MalformedURLException, CommsRouterException {
    ApiObjectRef queue = queueService.create(newCreateQueueArg("1==1", "desctiption_one"), "01");

    ApiObjectRef plan =
        planService.create(newCreatePlanArg("description_one", "1==1", queue.getRef()), "01");

    taskService.create(newCreateTaskArg(null, "https://test.com", plan.getRef()), "01");
    PaginatedList<TaskDto> task = taskService.list(
        new PagingRequest("01", null, 10, null, null));
    assertEquals(task.getList().get(0).getCallbackUrl(), "https://test.com");
  }

}
