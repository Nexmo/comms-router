/* 
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.commsrouter.webservice;

import com.softavail.commsrouter.api.interfaces.AgentService;
import com.softavail.commsrouter.api.interfaces.PlanService;
import com.softavail.commsrouter.api.interfaces.QueueService;
import com.softavail.commsrouter.api.interfaces.RouterService;
import com.softavail.commsrouter.api.interfaces.SkillService;
import com.softavail.commsrouter.api.interfaces.TaskService;
import com.softavail.commsrouter.api.service.CoreAgentService;
import com.softavail.commsrouter.api.service.CorePlanService;
import com.softavail.commsrouter.api.service.CoreQueueService;
import com.softavail.commsrouter.api.service.CoreRouterService;
import com.softavail.commsrouter.api.service.CoreSkillService;
import com.softavail.commsrouter.api.service.CoreTaskService;
import com.softavail.commsrouter.app.AppContext;
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

    bind(context.svc.task)
        .to(TaskService.class)
        .to(CoreTaskService.class);

    bind(context.svc.plan)
        .to(PlanService.class)
        .to(CorePlanService.class);

    bind(context.svc.queue)
        .to(QueueService.class)
        .to(CoreQueueService.class);

    bind(context.svc.agent)
        .to(AgentService.class)
        .to(CoreAgentService.class);

    bind(context.svc.skill)
        .to(SkillService.class)
        .to(CoreSkillService.class);

    bind(context.svc.router)
        .to(RouterService.class)
        .to(CoreRouterService.class);

  }

}
