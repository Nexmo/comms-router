/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.service.CoreAgentService;
import com.softavail.commsrouter.api.service.CorePlanService;
import com.softavail.commsrouter.api.service.CoreQueueService;
import com.softavail.commsrouter.api.service.CoreRouterService;
import com.softavail.commsrouter.api.service.CoreTaskService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.app.TaskDispatcher;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;

/**
 * @author G.Ivanov
 */
public class TestBase {

    protected static EntityManagerFactory emf;
    protected static EntityManager em;
    protected Router testRouter;

    protected static CoreQueueService queueService;
    protected static CoreTaskService taskService;
    protected static CoreAgentService agentService;
    protected static CoreRouterService routerService;
    protected static CorePlanService planService;
    protected static AppContext app;

    //Connects to the in-memory h2 database.
    @Before
    public void initDb() throws CommsRouterException {
        emf = Persistence.createEntityManagerFactory("mnf-pu-test");
        em = emf.createEntityManager();
        //Inserting two dummy routers into the database
        createRouter("name_one", "description_one", "01");
        createRouter("name_two", "description_two", "02");
    }

    @BeforeClass
    public static void setTestCoreQueueService() {
        CommsRouterEvaluator ev = new CommsRouterEvaluator();
        JpaDbFacade db = new JpaDbFacade("mnf-pu-test");
        TaskDispatcher td = new TaskDispatcher(null, null, null);
        EntityMappers enm = new EntityMappers();
        app = new AppContext(db, ev, td, enm);
        queueService = new CoreQueueService(app);
        taskService = new CoreTaskService(app);
        agentService = new CoreAgentService(app);
        routerService = new CoreRouterService(app);
        planService = new CorePlanService(app);
    }

    @After
    public void closeDb() {
        em.close();
        emf.close();
    }

    //Creates and adds a new router object to the DB
    public static void createRouter(String name, String description, String id) {
        em.getTransaction().begin();
        Router r = new Router();
        r.setDescription(description);
        r.setName(name);
        r.setId(id);
        r.setVersion(1);
        em.persist(r);
        em.getTransaction().commit();
    }

    public CreateAgentArg returnNewCreateAgentArg(String address) {
        AttributeGroupDto aGroupDto = new AttributeGroupDto();
        CreateAgentArg args = new CreateAgentArg();
        args.setAddress(address);
        args.setCapabilities(aGroupDto);
        return args;
    }

    public UpdateAgentArg returnNewUpdateAgentArg(String address, AgentState status) {
        AttributeGroupDto aGroupDto = new AttributeGroupDto();
        UpdateAgentArg args = new UpdateAgentArg();
        args.setAddress(address);
        args.setCapabilities(aGroupDto);
        args.setState(status);
        return args;
    }

    public CreateQueueArg newCreateQueueArg(String predicate, String description) {

        CreateQueueArg args = new CreateQueueArg();
        args.setDescription(description);
        args.setPredicate(predicate);
        return args;

    }

    public CreatePlanArg returnNewCreatePlanArg(String description, String predicate, String queueId) {
        CreatePlanArg args = new CreatePlanArg();
        RuleDto rule = new RuleDto();
        rule.setPredicate(predicate);
        rule.setQueueId(queueId);
        rule.setTag("tag");
        List<RuleDto> rules = new ArrayList();
        rules.add(rule);
        args.setDescription(description);
        args.setRules(rules);
        return args;
    }

    public UpdatePlanArg returnNewUpdatePlanArg(String description, String predicate, String queueId) {
        UpdatePlanArg args = new UpdatePlanArg();
        RuleDto rule = new RuleDto();
        rule.setPredicate(predicate);
        rule.setQueueId(queueId);
        rule.setTag("tag");
        List<RuleDto> rules = new ArrayList();
        rules.add(rule);
        args.setDescription(description);
        args.setRules(rules);
        return args;
    }

    public UpdateQueueArg newUpdateQueueArg(String predicate, String description) {
        UpdateQueueArg args = new UpdateQueueArg();
        args.setDescription(description);
        args.setPredicate(predicate);
        return args;
    }

    public CreateRouterArg returnNewCreateRouterArg(String name, String description) {
        CreateRouterArg args = new CreateRouterArg();
        args.setDescription(description);
        args.setName(name);
        return args;
    }

    public UpdateRouterArg returnNewUpdateRouterArg(String name, String description) {
        UpdateRouterArg args = new UpdateRouterArg();
        args.setDescription(description);
        args.setName(name);
        return args;
    }

    public CreateTaskArg newCreateTaskArg(String queueId, String URL, String planId) throws MalformedURLException {

        CreateTaskArg args = new CreateTaskArg();
        args.setQueueId(queueId);
        args.setPriority(1L);
        URL url = new URL(URL);
        AttributeGroupDto requirements = new AttributeGroupDto();
        args.setRequirements(requirements);
        args.setCallbackUrl(url);
        args.setPlanId(planId);
        return args;
    }

    public UpdateTaskArg newUpdateTaskArg(long priority, TaskState state) throws MalformedURLException {

        UpdateTaskArg args = new UpdateTaskArg();
        args.setPriority(priority);
        args.setState(state);
        return args;
    }
}
