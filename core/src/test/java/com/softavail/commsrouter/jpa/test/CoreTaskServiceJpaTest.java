/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import java.net.MalformedURLException;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CoreTaskServiceJpaTest extends TestBase {

    @Test
    public void createTest() throws MalformedURLException, CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        QueueDto queue = queueService.create(newCreateQueueArg("predicate_one", "desctiption_one"), id);
        taskService.create(newCreateTaskArg(queue.getId(), "https://test.com", null), id);
        TaskDto task = taskService.get(id);
        assertEquals(task.getCallbackUrl(),"https://test.com");
    }

    @Test
    public void putTest() throws CommsRouterException, MalformedURLException {
        RouterObjectId id = new RouterObjectId("", "01");
        QueueDto queue = queueService.create(newCreateQueueArg("predicate_one", "desctiption_one"), id);
        taskService.create(newCreateTaskArg(queue.getId(), "https://test_one.com", null), id);
        taskService.put(newCreateTaskArg(queue.getId(), "https://test_two.com", null), id);

        TaskDto task = taskService.get(id);
        assertEquals(task.getCallbackUrl(),"https://test_two.com");
    }

    @Test
    public void updateTest() throws CommsRouterException, MalformedURLException {
        RouterObjectId id = new RouterObjectId("", "01");
        QueueDto queue = queueService.create(newCreateQueueArg("predicate_one", "desctiption_one"), id);
        taskService.create(newCreateTaskArg(queue.getId(), "https://test_one.com", null), id);
        taskService.update(newUpdateTaskArg(2, TaskState.completed), id);

        TaskDto task = taskService.get(id);
        assertEquals(task.getState(), TaskState.completed);
    }

    @Test
    public void updateContextTest() throws CommsRouterException, MalformedURLException {
        RouterObjectId id = new RouterObjectId("", "01");
        QueueDto queue = queueService.create(newCreateQueueArg("predicate_one", "desctiption_one"), id);
        taskService.create(newCreateTaskArg(queue.getId(), "https://test_one.com", null), id);

        UpdateTaskContext ctx = new UpdateTaskContext(id);
        taskService.update(ctx);

        TaskDto task = taskService.get(id);
        assertEquals(task.getState(), TaskState.waiting);
    }

    @Test(expected = IllegalArgumentException.class)
    public void createTest_() throws MalformedURLException, CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        QueueDto queue = queueService.create(newCreateQueueArg("predicate_one", "desctiption_one"), id);
        PlanDto plan = planService.create(returnNewCreatePlanArg("desctiption_one", "predicate_one", queue.getId()), id);
        taskService.create(newCreateTaskArg(null, "https://test.com", plan.getId()), id);
        TaskDto task = taskService.get(id);
        assertEquals(task.getCallbackUrl(), "https://test.com");
    }

}
