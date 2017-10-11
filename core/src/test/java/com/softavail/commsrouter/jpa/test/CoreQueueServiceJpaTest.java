/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.service.CoreQueueService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.app.TaskDispatcher;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CoreQueueServiceJpaTest extends TestBase {

    private static CoreQueueService queueService;
    private static AppContext app;

    @BeforeClass
    public static void setTestCoreAgentService() {

        CommsRouterEvaluator ev = new CommsRouterEvaluator();
        JpaDbFacade db = new JpaDbFacade("mnf-pu-test");
        TaskDispatcher td = new TaskDispatcher(null, null, null);
        EntityMappers enm = new EntityMappers();
        app = new AppContext(db, ev, td, enm);
        queueService = new CoreQueueService(app);

    }

    public CreateQueueArg returnNewCreateQueueArg(String predicate, String description) {

        CreateQueueArg args = new CreateQueueArg();
        args.setDescription(description);
        args.setPredicate(predicate);
        return args;

    }

    @Test
    public void createTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("id_one", "01");
        queueService.create(returnNewCreateQueueArg("predicate_one", "description_one"), id);

        QueueDto queue = queueService.get(id);

        assertEquals(queue.getDescription(),"description_one");
    }
    
    @Test
    public void putTest() throws CommsRouterException {
        RouterObjectId id = new RouterObjectId("id_one", "01");
        queueService.create(returnNewCreateQueueArg("predicate_one", "description_one"), id);
        QueueDto queueBefore = queueService.get(id);
        queueService.put(returnNewCreateQueueArg("predicate_two", "description_two"), id);
        QueueDto queueAfter = queueService.get(id);

        assertNotEquals(queueAfter, queueBefore);
        
    }

    @Test
    public void updateTest() throws CommsRouterException {
        //Test Code Goes Here
    }


}
