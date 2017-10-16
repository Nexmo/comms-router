package com.softavail.commsrouter.webservice;

import com.softavail.commsrouter.api.interfaces.AgentService;
import com.softavail.commsrouter.api.interfaces.PlanService;
import com.softavail.commsrouter.api.interfaces.QueueService;
import com.softavail.commsrouter.api.interfaces.RouterService;
import com.softavail.commsrouter.api.interfaces.TaskService;
import com.softavail.commsrouter.api.service.CoreAgentService;
import com.softavail.commsrouter.api.service.CorePlanService;
import com.softavail.commsrouter.api.service.CoreQueueService;
import com.softavail.commsrouter.api.service.CoreRouterService;
import com.softavail.commsrouter.api.service.CoreTaskService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.app.TaskDispatcher;
import com.softavail.commsrouter.providers.ClientFactory;
import org.glassfish.hk2.utilities.binding.AbstractBinder;

import javax.inject.Singleton;
import javax.ws.rs.client.Client;

/**
 * Created by @author mapuo on 01.09.17.
 */
public class ApplicationBindings extends AbstractBinder {

  private final AppContext context;

  public ApplicationBindings(AppContext context) {
    this.context = context;
  }

  @Override
  protected void configure() {

    bindFactory(ClientFactory.class)
        .to(Client.class)
        .in(Singleton.class);

    bind(new CoreTaskService(context))
        .to(TaskService.class)
        .to(CoreTaskService.class);

    bind(new CorePlanService(context))
        .to(PlanService.class)
        .to(CorePlanService.class);

    bind(new CoreQueueService(context))
        .to(QueueService.class)
        .to(CoreQueueService.class);

    bind(new CoreAgentService(context))
        .to(AgentService.class)
        .to(CoreAgentService.class);

    bind(new CoreRouterService(context))
        .to(RouterService.class)
        .to(CoreRouterService.class);

    bind(context.taskDispatcher)
        .to(TaskDispatcher.class);

  }

}
