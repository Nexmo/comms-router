/*
 * Copyright 2018 SoftAvail Inc.
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

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.app.AppContext;

/**
 *
 * @author ikrustev
 */
public class Services {

  public final CoreTaskService task;
  public final CorePlanService plan;
  public final CoreQueueService queue;
  public final CoreAgentService agent;
  public final CoreSkillService skill;
  public final CoreRouterService router;

  public Services(AppContext context) {
    this.task = new CoreTaskService(context);
    this.plan = new CorePlanService(context);
    this.queue = new CoreQueueService(context);
    this.agent = new CoreAgentService(context);
    this.skill = new CoreSkillService(context);
    this.router = new CoreRouterService(context);
  }

}
