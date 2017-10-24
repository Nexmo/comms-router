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

import java.util.List;
import java.util.stream.Collectors;

/**
 *
 * @author ikrustev
 */
public class PlanMapper extends RouterObjectEntityMapper<PlanDto, Plan> {

  @Override
  public PlanDto toDto(Plan jpa) {
    PlanDto dto = new PlanDto();
    copyId(dto, jpa);
    dto.setDescription(jpa.getDescription());
    dto.setRules(toDtoRules(jpa.getRules()));
    dto.setDefaultRoute(toDto(jpa.getDefaultRoute()));
    return dto;
  }

  private RouteDto toDto(Route jpa) {
    RouteDto dto = new RouteDto();
    dto.setQueueId(jpa.getQueueId());
    dto.setPriority(jpa.getPriority());
    dto.setTimeout(jpa.getTimeout());
    return dto;
  }

  private RuleDto toDto(Rule jpa) {
    RuleDto dto = new RuleDto();
    dto.setPredicate(jpa.getPredicate());
    dto.setTag(jpa.getTag());
    dto.setRoutes(toDtoRoutes(jpa.getRoutes()));
    return dto;
  }

  public Route fromDto(RouteDto dto) {
    Route route = new Route();
    route.setQueueId(dto.getQueueId());
    route.setPriority(dto.getPriority());
    route.setTimeout(dto.getTimeout());
    return route;
  }

  private Rule fromDto(RuleDto dto) {
    Rule rule = new Rule();
    rule.setPredicate(dto.getPredicate());
    rule.setTag(dto.getTag());
    addDtoRoutes(rule, dto.getRoutes());
    return rule;
  }

  private List<RuleDto> toDtoRules(List<Rule> jpaRules) {
    return jpaRules.stream().map(this::toDto).collect(Collectors.toList());
  }

  public void addDtoRules(Plan plan, List<RuleDto> dtoRules) {
    if (dtoRules == null) {
      return;
    }
    dtoRules.forEach(dto -> plan.addRule(fromDto(dto)));
  }

  private List<RouteDto> toDtoRoutes(List<Route> jpaRoutes) {
    return jpaRoutes.stream().map(this::toDto).collect(Collectors.toList());
  }

  public void addDtoRoutes(Rule rule, List<RouteDto> dtoRoutes) {
    if (dtoRoutes == null) {
      return;
    }
    dtoRoutes.forEach(dto -> rule.addRoute(fromDto(dto)));
  }

}
