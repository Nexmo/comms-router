/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
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
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.service.CoreAgentService;
import com.softavail.commsrouter.api.service.CorePlanService;
import com.softavail.commsrouter.api.service.CoreQueueService;
import com.softavail.commsrouter.api.service.CoreRouterService;
import com.softavail.commsrouter.api.service.CoreSkillService;
import com.softavail.commsrouter.api.service.CoreTaskService;
import com.softavail.commsrouter.api.service.SkillValidator;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.app.CoreConfiguration;
import com.softavail.commsrouter.app.TaskDispatcher;
import com.softavail.commsrouter.domain.AttributeGroup;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.domain.RouterConfig;
import com.softavail.commsrouter.domain.dto.mappers.AttributesMapper;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.eval.CommsRouterEvaluatorFactory;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import static com.softavail.commsrouter.jpa.test.TestBase.skillValidator;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

/**
 * @author G.Ivanov
 */
public class TestBase {

  protected static EntityManagerFactory emf;
  protected static EntityManager em;
  protected Router testRouter;
  // All services
  protected static CoreQueueService queueService;
  protected static CoreTaskService taskService;
  protected static CoreAgentService agentService;
  protected static CoreRouterService routerService;
  protected static CorePlanService planService;
  protected static CoreSkillService skillService;
  protected static SkillValidator skillValidator;
  protected static AppContext app;

  // Connects to the in-memory h2 database.
  @Before
  public void initDb() throws CommsRouterException {
    // Inserting two test routers into the database
    emf = Persistence.createEntityManagerFactory("mnf-pu-test");
    em = emf.createEntityManager();
    createRouter("name_one", "description_one", "01");
    createRouter("name_two", "description_two", "02");
  }

  @BeforeClass
  public static void setTestCoreQueueService() {
    CommsRouterEvaluatorFactory evf = new CommsRouterEvaluatorFactory();
    JpaDbFacade db = new JpaDbFacade( "mnf-pu-test");
    TaskDispatcher td = new TaskDispatcher(db, null, new CoreConfiguration() {
      @Override
      public Integer getBackoffDelay() {
        return CoreConfiguration.DEFAULT.getBackoffDelay();
      }

      @Override
      public Integer getBackoffDelayMax() {
        return CoreConfiguration.DEFAULT.getBackoffDelayMax();
      }

      @Override
      public Integer getJitter() {
        return CoreConfiguration.DEFAULT.getJitter();
      }

      @Override
      public Integer getDispatcherThreadPoolSize() {
        return 20;
      }

      @Override
      public Integer getDispatcherThreadShutdownDelay() {
        return CoreConfiguration.DEFAULT.getDispatcherThreadShutdownDelay();
      }

      @Override
      public Integer getQueueProcessRetryDelay() {
        return CoreConfiguration.DEFAULT.getQueueProcessRetryDelay();
      }

      @Override
      public Long getQueueProcessorEvictionDelay() {
        return CoreConfiguration.DEFAULT.getQueueProcessorEvictionDelay();
      }

      @Override
      public Integer getJpaLockRetryCount() {
        return CoreConfiguration.DEFAULT.getJpaLockRetryCount();
      }

      @Override
      public Boolean getApiEnableExpressionSkillValidation() {
        return false;
      }

      @Override
      public Boolean getApiEnableAgentCapabilitiesValidation() {
        return false;
      }

      @Override
      public Boolean getApiEnableTaskRequirementsValidation() {
        return false;
      }
    }, null);
    EntityMappers enm = new EntityMappers();
    app = new AppContext(db, evf, td, enm, CoreConfiguration.DEFAULT);
    // Instantiating all of the services
    queueService = new CoreQueueService(app);
    taskService = new CoreTaskService(app);
    agentService = new CoreAgentService(app);
    routerService = new CoreRouterService(app);
    planService = new CorePlanService(app);
    skillService = new CoreSkillService(app);
    skillValidator = new SkillValidator(skillService);
  }

  @After
  public void closeDb() {
    em.close();
    emf.close();
  }

  // Creates and adds a new router object to the DB
  public static void createRouter(String name, String description, String ref) {
    em.getTransaction().begin();
    Router r = new Router();
    r.setDescription(description);
    r.setName(name);
    r.setRef(ref);
    em.persist(r);
    RouterConfig rc = new RouterConfig();
    rc.setRouter(r);
    r.setConfig(rc);
    em.persist(rc);
    em.getTransaction().commit();
  }

  public CreateAgentArg newCreateAgentArg(String address) {
    // Creating test attributeGroup and attributes
    AttributeGroup aGroup = new AttributeGroup();
    aGroup.add("name", "value");
    aGroup.setId(5L);
    AttributesMapper mapper = new AttributesMapper();
    AttributeGroupDto aGroupDto = mapper.toDto(aGroup);
    // Creating test agent arguments
    CreateAgentArg args = new CreateAgentArg();
    args.setAddress(address);
    args.setCapabilities(aGroupDto);
    return args;
  }

  public UpdateAgentArg newUpdateAgentArg(String address, AgentState status) {
    // Creating test attributeGroup and attributes
    AttributeGroup aGroup = new AttributeGroup();
    aGroup.add("name", "value");
    aGroup.setId(5L);
    AttributesMapper mapper = new AttributesMapper();
    AttributeGroupDto aGroupDto = mapper.toDto(aGroup);
    // Creating test agent arguments
    UpdateAgentArg args = new UpdateAgentArg();
    args.setAddress(address);
    args.setCapabilities(aGroupDto);
    args.setState(status);
    return args;
  }

  public CreateQueueArg newCreateQueueArg(String predicate, String description) {
    // Creating test queue arguments
    CreateQueueArg args = new CreateQueueArg();
    args.setDescription(description);
    args.setPredicate(predicate);
    return args;
  }

  public UpdateQueueArg newUpdateQueueArg(String predicate, String description) {
    // Creating test queue arguments
    UpdateQueueArg args = new UpdateQueueArg();
    args.setDescription(description);
    args.setPredicate(predicate);
    return args;
  }

  public CreatePlanArg newCreatePlanArg(String description, String predicate, String queueId) {
    // Creating test rules
    RuleDto rule = new RuleDto();
    rule.setPredicate(predicate);
    RouteDto route = new RouteDto();
    route.setQueueRef(queueId);
    rule.getRoutes().add(route);
    rule.setTag("tag");
    List<RuleDto> rules = new ArrayList<>();
    rules.add(rule);
    // Creating test plan arguments
    CreatePlanArg args = new CreatePlanArg();
    args.setDescription(description);
    args.setRules(rules);
    args.setDefaultRoute(route);
    return args;
  }

  public UpdatePlanArg newUpdatePlanArg(String description) {
    // Creating test plan arguments
    UpdatePlanArg args = new UpdatePlanArg();
    args.setDescription(description);
    return args;
  }

  public CreateRouterArg newCreateRouterArg(String name, String description) {
    // Creating test router arguments
    CreateRouterArg args = new CreateRouterArg();
    args.setDescription(description);
    args.setName(name);
    return args;
  }

  public UpdateRouterArg newUpdateRouterArg(String name, String description) {
    // Creating test router arguments
    UpdateRouterArg args = new UpdateRouterArg();
    args.setDescription(description);
    args.setName(name);
    return args;
  }

  public CreateTaskArg newCreateTaskArg(String queueId, String URL, String planId)
      throws MalformedURLException {
    // Creating test task arguments
    CreateTaskArg args = new CreateTaskArg();
    args.setQueueRef(queueId);
    URL url = new URL(URL);
    AttributeGroupDto requirements = new AttributeGroupDto();
    args.setRequirements(requirements);
    args.setUserContext(requirements);
    args.setCallbackUrl(url);
    args.setPlanRef(planId);
    return args;
  }

  public UpdateTaskArg newUpdateTaskArg(long priority, TaskState state)
      throws MalformedURLException {
    // Creating test task arguments
    UpdateTaskArg args = new UpdateTaskArg();
    args.setState(state);
    return args;
  }

  public UpdateTaskContext newUpdateTaskContext() {
    AttributeGroup aGroup = new AttributeGroup();
    aGroup.add("name", "value");
    aGroup.setId(5L);
    AttributesMapper mapper = new AttributesMapper();
    AttributeGroupDto aGroupDto = mapper.toDto(aGroup);

    UpdateTaskContext ctx = new UpdateTaskContext();
    ctx.setUserContext(aGroupDto);
    return ctx;
  }
}
