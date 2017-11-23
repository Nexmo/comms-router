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
    copyRef(dto, jpa);
    dto.setDescription(jpa.getDescription());
    dto.setRules(toDtoRules(jpa.getRules()));
    dto.setDefaultRoute(toDto(jpa.getDefaultRoute()));
    return dto;
  }

  private RouteDto toDto(Route jpa) {
    RouteDto dto = new RouteDto();
    if (jpa.getQueue() != null) {
      dto.setQueueRef(jpa.getQueue().getRef());
    }
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
    route.setPriority(dto.getPriority());
    route.setTimeout(dto.getTimeout());
    return route;
  }

  public Rule fromDto(RuleDto dto) {
    Rule rule = new Rule();
    rule.setPredicate(dto.getPredicate());
    rule.setTag(dto.getTag());
    return rule;
  }

  private List<RuleDto> toDtoRules(List<Rule> jpaRules) {
    return jpaRules.stream().map(this::toDto).collect(Collectors.toList());
  }

  private List<RouteDto> toDtoRoutes(List<Route> jpaRoutes) {
    return jpaRoutes.stream().map(this::toDto).collect(Collectors.toList());
  }

}
