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

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Route;
import com.softavail.commsrouter.domain.Rule;

import java.util.List;
import javax.persistence.EntityManager;

/**
 *
 * @author ikrustev
 */
class PlanResolver {

  protected final AppContext app;
  private final EntityManager em;
  private final Plan plan;

  public PlanResolver(AppContext app, EntityManager em, Plan plan) {
    this.app = app;
    this.em = em;
    this.plan = plan;
  }

  public static PlanResolver create(AppContext app, EntityManager em, Plan plan) {
    return new PlanResolver(app, em, plan);
  }

  public PlanResolver addDtoRules(List<RuleDto> dtoRules) throws NotFoundException {
    if (dtoRules == null) {
      return this;
    }
    for (RuleDto dto : dtoRules) {
      Rule rule = app.entityMapper.plan.fromDto(dto);
      addDtoRoutes(rule, dto.getRoutes());
      plan.addRule(rule);
    }
    return this;
  }

  public PlanResolver setDefaultDtoRoute(RouteDto routeDto) throws NotFoundException {
    plan.setDefaultRoute(resolveRoute(routeDto));
    return this;
  }

  private void addDtoRoutes(Rule rule, List<RouteDto> dtoRoutes) throws NotFoundException {

    if (dtoRoutes == null) {
      return;
    }
    for (RouteDto routeDto : dtoRoutes) {
      rule.addRoute(resolveRoute(routeDto));
    }
  }

  private Route resolveRoute(RouteDto routeDto)
      throws NotFoundException {

    Route route = app.entityMapper.plan.fromDto(routeDto);
    if (routeDto.getQueueRef() != null) {
      Queue queue = app.db.queue
          .get(em, new RouterObjectRef(routeDto.getQueueRef(), plan.getRouter().getRef()));
      route.setQueue(queue);
    }
    return route;
  }

}
