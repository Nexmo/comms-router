/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CoreQueueServiceJpaTest extends TestBase {

    @Test
    public void createTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        queueService.create(newCreateQueueArg("predicate_one", "description_one"), id);
        QueueDto queue = queueService.get(id);
        assertEquals(queue.getDescription(),"description_one");
    }
    
    @Test
    public void putTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        queueService.create(newCreateQueueArg("predicate_one", "description_one"), id);
        QueueDto queueBefore = queueService.get(id);
        queueService.put(newCreateQueueArg("predicate_two", "description_two"), id);
        QueueDto queueAfter = queueService.get(id);
        assertNotEquals(queueAfter, queueBefore);
    }

    @Test
    public void updateTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("", "01");
        queueService.create(newCreateQueueArg("predicate_one", "description_one"), id);
        QueueDto queueBefore = queueService.get(id);
        queueService.update(newUpdateQueueArg("predicate_two", "description_two"), id);
        QueueDto queueAfter = queueService.get(id);
        assertNotEquals(queueAfter, queueBefore);
    }

    @Test
    public void getQueueSizeTest() throws CommsRouterException, MalformedURLException {
        RouterObjectId id = new RouterObjectId("", "01");
        QueueDto queue = queueService.create(newCreateQueueArg("predicate_one", "description_one"), id);
        CreateTaskArg args = new CreateTaskArg();
        args.setQueueId(id.getId());
        args.setPriority(1L);
        URL url = new URL("https://test.com");
        args.setCallbackUrl(url);
        taskService.create(args, id);
        RouterObjectId idd = new RouterObjectId(queue.getId(), queue.getRouterId());
        long size = queueService.getQueueSize(idd);
        assertEquals(size,1);
    }

    @Test
    public void getTasksTest() throws CommsRouterException, MalformedURLException {
        RouterObjectId id = new RouterObjectId("", "01");
        QueueDto queue = queueService.create(newCreateQueueArg("predicate_one", "description_one"), id);
        CreateTaskArg args = new CreateTaskArg();
        args.setQueueId(id.getId());
        args.setPriority(1L);
        URL url = new URL("https://test.com");
        args.setCallbackUrl(url);
        taskService.create(args, id);
        RouterObjectId idd = new RouterObjectId(queue.getId(), queue.getRouterId());
        List<TaskDto> tasks = (List)queueService.getTasks(idd);
        assertEquals(tasks.size(),1);
        assertEquals(tasks.get(0).getCallbackUrl(),"https://test.com");
    }


}
