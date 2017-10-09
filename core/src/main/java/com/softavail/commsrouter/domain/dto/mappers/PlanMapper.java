/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Route;
import com.softavail.commsrouter.domain.Rule;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author ikrustev
 */
public class PlanMapper extends EntityMapper<PlanDto, Plan> {

  @Override
  public PlanDto toDto(Plan jpa) {
    PlanDto dto = new PlanDto();
    copyId(dto, jpa);
    dto.setDescription(jpa.getDescription());
    dto.setRules(toDtoRules(jpa.getRules()));
    dto.setDefaultRoute(toRouteDto(jpa.getDefaultRoute()));
    return dto;
  }

  private RouteDto toRouteDto(Route jpa) {
    RouteDto dto = new RouteDto();
    dto.setPredicate(jpa.getPredicate());
    dto.setQueueId(jpa.getQueueId());
    dto.setPriority(jpa.getPriority());
    dto.setTimeout(jpa.getTimeout());
    return dto;
  }

  public Route fromRouteDto(RouteDto dto) {
    Route route = new Route();
    route.setPredicate(dto.getPredicate());
    route.setQueueId(dto.getQueueId());
    route.setPriority(dto.getPriority());
    route.setTimeout(dto.getTimeout());
    return route;
  }

  private RuleDto toRuleDto(Rule jpa) {
    RuleDto dto = new RuleDto();
    dto.setPredicate(jpa.getPredicate());
    dto.setTag(jpa.getTag());
    dto.setRoutes(toDtoRoutes(jpa.getRoutes()));
    return dto;
  }

  private Rule fromRuleDto(RuleDto dto) {
    Rule rule = new Rule();
    rule.setPredicate(dto.getPredicate());
    rule.setTag(dto.getTag());
    addDtoRoutes(rule, dto.getRoutes());
    return rule;
  }

  private List<RuleDto> toDtoRules(List<Rule> jpaRules) {
    List<RuleDto> dtoRules = new ArrayList<>();
    jpaRules.stream().forEach(jpa -> dtoRules.add(toRuleDto(jpa)));
    return dtoRules;
  }

  public void addDtoRules(Plan plan, List<RuleDto> dtoRules) {
    if (dtoRules == null) {
      return;
    }
    dtoRules.stream().forEach(dto -> plan.addRule(fromRuleDto(dto)));
  }

  private List<RouteDto> toDtoRoutes(List<Route> jpaRoutes) {
    List<RouteDto> dtoRoutes = new ArrayList<>();
    jpaRoutes.stream().forEach(jpa -> dtoRoutes.add(toRouteDto(jpa)));
    return dtoRoutes;
  }

  public void addDtoRoutes(Rule rule, List<RouteDto> dtoRoutes) {
    if (dtoRoutes == null) {
      return;
    }
    dtoRoutes.stream().forEach(dto -> rule.addRoute(fromRouteDto(dto)));
  }

}
