/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package playground;

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.domain.RouterObject;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.LongAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.service.CoreAgentService;
import com.softavail.commsrouter.api.service.CorePlanService;
import com.softavail.commsrouter.api.service.CoreQueueService;
import com.softavail.commsrouter.api.service.CoreRouterObjectService;
import com.softavail.commsrouter.api.service.CoreRouterService;
import com.softavail.commsrouter.api.service.CoreTaskService;
import com.softavail.commsrouter.app.TaskDispatcher;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.AttributeGroup;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Rule;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Route;
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import com.softavail.commsrouter.jpa.VoidTransactionLogic;
import java.net.MalformedURLException;
import java.net.URL;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author ikrustev
 */
public class JpaPlayground implements AutoCloseable {

  private final JpaDbFacade db = new JpaDbFacade();
  private final EntityMappers entityMapper = new EntityMappers();
  private final CommsRouterEvaluator evaluator = new CommsRouterEvaluator();
  private final TaskDispatcher taskDispatcher = new TaskDispatcher(db, (assignment) -> {
    System.out
        .println("Task: " + assignment.getTask() + " assigned to Agent: " + assignment.getAgent());
  }, entityMapper);
  private final AppContext app = new AppContext(db, evaluator, taskDispatcher, entityMapper);
  private final CoreRouterService routerService = new CoreRouterService(app);
  private final CoreQueueService queueService = new CoreQueueService(app);
  private final CoreTaskService taskService = new CoreTaskService(app);
  private final CorePlanService planService = new CorePlanService(app);
  private final CoreAgentService agentService = new CoreAgentService(app);

  @Override
  public void close() throws Exception {
    taskDispatcher.close();
    db.close();
  }

  private <ENTITYT extends ApiObjectId> void printList(List<ENTITYT> list) {
    list.stream().forEach(e -> System.out.println(e.getClass().getSimpleName() + ": " + e));
  }

  private void testRouter(String id, CoreRouterService service) throws CommsRouterException {

    System.out.println("testApiObject:" + service.getDtoEntityClass().getSimpleName());
    printList(service.list());
    service.delete(id);
    printList(service.list());

    CreateRouterArg createRouterArg = new CreateRouterArg();
    service.create(createRouterArg);

    printList(service.list());
  }

  private <DTOENTITYT extends RouterObjectId, ENTITYT extends RouterObject, SERVICET extends CoreRouterObjectService<DTOENTITYT, ENTITYT>> void testRouterObject(
      RouterObjectId routerObject, SERVICET service, VoidTransactionLogic transaction)
      throws CommsRouterException {

    System.out.println("testRouterObject:" + service.getDtoEntityClass().getSimpleName());
    printList(service.list(routerObject.getRouterId()));
    service.delete(routerObject);
    printList(service.list(routerObject.getRouterId()));
    db.transactionManager.execute(transaction);
    printList(service.list(routerObject.getRouterId()));
  }

  static interface Creator {
    void run() throws CommsRouterException;
  }

  private <DTOENTITYT extends RouterObjectId, ENTITYT extends RouterObject, SERVICET extends CoreRouterObjectService<DTOENTITYT, ENTITYT>> void testRouterObject(
      RouterObjectId routerObject, SERVICET service, Creator creator) throws CommsRouterException {

    System.out.println("testRouterObject:" + service.getDtoEntityClass().getSimpleName());
    printList(service.list(routerObject.getRouterId()));
    service.delete(routerObject);
    printList(service.list(routerObject.getRouterId()));
    creator.run();
    printList(service.list(routerObject.getRouterId()));
  }

  private void go() throws InstantiationException, IllegalAccessException, NotFoundException,
      CommsRouterException {

    testRouter("router-id", routerService);

    testRouterObject(RouterObjectId.builder().setRouterId("router-id").setId("queue-id1").build(),
        queueService, (em) -> {
          Queue queue = new Queue();
          queue.setRouterId("router-id");
          queue.setId("queue-id1");
          em.persist(queue);
        });

    testRouterObject(RouterObjectId.builder().setRouterId("router-id").setId("queue-id2").build(),
        queueService, (em) -> {
          Queue queue = new Queue();
          queue.setRouterId("router-id");
          queue.setId("queue-id2");
          em.persist(queue);
        });

    testRouterObject(RouterObjectId.builder().setRouterId("router-id").setId("queue-id-6").build(),
        queueService, (em) -> {
          Queue queue = new Queue();
          queue.setRouterId("router-id");
          queue.setId("queue-id-6");
          queue.setPredicate("CONTAINS(language, 'es')");
          em.persist(queue);
        });

    testRouterObject(RouterObjectId.builder().setRouterId("router-id").setId("queue-id-5").build(),
        queueService, (em) -> {
          Queue queue = new Queue();
          queue.setRouterId("router-id");
          queue.setId("queue-id-5");
          queue.setPredicate("language == 'en'");
          em.persist(queue);
        });

    testRouterObject(RouterObjectId.builder().setRouterId("router-id").setId("plan-id").build(),
        planService, (em) -> {
          Plan plan = new Plan();
          plan.setDescription("my plan");

          Route route;
          route = new Route();
          route.setQueueId("queue-id-6");
          Rule rule;
          rule = new Rule();
          rule.setPredicate("language == 'es'");
          rule.setTag("t6");
          rule.getRoutes().add(route);
          plan.addRule(rule);

          route = new Route();
          route.setQueueId("queue-id-5");
          rule = new Rule();
          rule.setPredicate("toLowerCase(language)=='en'");
          rule.setTag("t5");
          rule.getRoutes().add(route);
          plan.addRule(rule);

          route = new Route();
          route.setQueueId("queue-id2");
          rule = new Rule();
          rule.setPredicate("B < 7");
          rule.setTag("t2");
          rule.getRoutes().add(route);
          plan.addRule(rule);

          route = new Route();
          route.setQueueId("queue-id-5");
          plan.setDefaultRoute(route);

          plan.setId("plan-id");
          plan.setRouterId("router-id");

          em.persist(plan);

        });

    testRouterObject(RouterObjectId.builder().setRouterId("router-id").setId("agent-id").build(),
        agentService, (em) -> {
          Queue queue1 = db.queue.get(em,
              RouterObjectId.builder().setRouterId("router-id").setId("queue-id1").build());
          Queue queue2 = db.queue.get(em,
              RouterObjectId.builder().setRouterId("router-id").setId("queue-id2").build());
          Agent agent = new Agent();
          AttributeGroup capabilities = new AttributeGroup();
          capabilities.add("language", "en");
          capabilities.add("language", "es");
          capabilities.add("favorite_numbers", 42L);
          capabilities.add("favorite_numbers", 24L);
          capabilities.add("is_supervisor", true);
          agent.setCapabilities(capabilities);
          agent.setState(AgentState.ready);
          agent.getQueues().add(queue1);
          agent.getQueues().add(queue2);
          agent.setId("agent-id");
          agent.setRouterId("router-id");
          em.persist(agent);
        });

    testRouterObject(RouterObjectId.builder().setRouterId("router-id").setId("task-id").build(),
        taskService, () -> {
          CreateTaskArg createTaskArg = new CreateTaskArg();
          AttributeGroupDto requrements = new AttributeGroupDto();
          requrements.put("language", new StringAttributeValueDto("en"));
          requrements.put("color", new StringAttributeValueDto("red"));
          requrements.put("price", new LongAttributeValueDto(42L));
          createTaskArg.setRequirements(requrements);
          createTaskArg.setQueueId("queue-id1");
          RouterObjectId objectId =
              RouterObjectId.builder().setId("task-id").setRouterId("router-id").build();
          try {
            createTaskArg.setCallbackUrl(new URL("http://localhost"));
          } catch (MalformedURLException ex) {
            throw new RuntimeException("Bad URL");
          }
          taskService.create(createTaskArg, objectId);
        });

    // taskDispatcher.dispatchTask("task-id");
    // sleep();
    taskDispatcher.dispatchAgent("agent-id");
    sleep();
    UpdateTaskArg updateTaskArg = new UpdateTaskArg();
    updateTaskArg.setState(TaskState.completed);
    RouterObjectId objectId =
        RouterObjectId.builder().setId("task-id").setRouterId("router-id").build();
    taskService.update(updateTaskArg, objectId);
    sleep();
  }

  private void sleep() {
    try {
      Thread.sleep(1000);
    } catch (InterruptedException ex) {
      Logger.getLogger(JpaPlayground.class.getName()).log(Level.SEVERE, null, ex);
    }
  }

  public static void main(String[] args) {

    try (JpaPlayground jpaTest = new JpaPlayground()) {

      jpaTest.go();

    } catch (Exception ex) {
      System.out.println("Exception: " + ex);
      ex.printStackTrace();
    }

  }

}
