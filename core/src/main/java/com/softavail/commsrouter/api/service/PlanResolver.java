/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
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
    if (routeDto.getQueueId() != null) {
      Queue queue =
          app.db.queue.get(em, new RouterObjectId(routeDto.getQueueId(), plan.getRouter().getId()));
      route.setQueue(queue);
    }
    return route;
  }

}
