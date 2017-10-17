/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import java.net.MalformedURLException;
import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CoreTaskServiceJpaTest extends TestBase {

    //Testing the create method that takes a RouterObjectId
    @Test
    public void createTest() throws MalformedURLException, CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        ApiObjectId queue = queueService.create(newCreateQueueArg("1=1", "desctiption_one"), id);
        taskService.create(newCreateTaskArg(queue.getId(), "https://test.com", null), id);
        TaskDto task = taskService.get(id);
        assertEquals(task.getCallbackUrl(),"https://test.com");
    }

    //Testing the create method that takes a String routerId
    @Test
    public void createTestTwo() throws MalformedURLException, CommsRouterException {
        ApiObjectId queue = queueService.create(newCreateQueueArg("1=1", "desctiption_one"), "01");
        taskService.create(newCreateTaskArg(queue.getId(), "https://test.com", null), "01");
        List<TaskDto> task = taskService.list("01");
        assertEquals(task.get(0).getCallbackUrl(), "https://test.com");
    }

    //Testing the update method
    @Test
    public void updateTest() throws CommsRouterException, MalformedURLException {
        RouterObjectId id = new RouterObjectId("", "01");
        ApiObjectId queue = queueService.create(newCreateQueueArg("1=1", "desctiption_one"), id);
        taskService.create(newCreateTaskArg(queue.getId(), "https://test_one.com", null), id);
        taskService.update(newUpdateTaskArg(2, TaskState.completed), id);
        TaskDto task = taskService.get(id);
        assertEquals(task.getState(), TaskState.completed);
    }

    //Testing the update method that updates the TaskContext
    @Test
    public void updateContextTest() throws CommsRouterException, MalformedURLException {
        RouterObjectId id = new RouterObjectId("", "01");
        ApiObjectId queue = queueService.create(newCreateQueueArg("1=1", "desctiption_one"), id);
        taskService.create(newCreateTaskArg(queue.getId(), "https://test_one.com", null), id);
        UpdateTaskContext ctx = new UpdateTaskContext();
        taskService.update(ctx, id);
        TaskDto task = taskService.get(id);
        assertEquals(task.getState(), TaskState.waiting);
    }

    //Passing a state != to completed should throw a BadValueException
    @Test(expected = BadValueException.class)
    public void exceptionTestTwo() throws MalformedURLException, CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        ApiObjectId queue = queueService.create(newCreateQueueArg("1=1", "desctiption_one"), id);
        taskService.create(newCreateTaskArg(queue.getId(), "https://test_one.com", null), id);
        taskService.update(newUpdateTaskArg(2, TaskState.waiting), id);
    }

}
