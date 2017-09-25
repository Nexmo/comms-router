/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Rule;
import com.softavail.commsrouter.util.Uuid;

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
    dto.setRules(fromJpaRules(jpa.getRules()));
    return dto;
  }

  private RuleDto fromJpa(Rule jpa) {
    RuleDto dto = new RuleDto();
    dto.setPredicate(jpa.getPredicate());
    dto.setQueueId(jpa.getQueueId());
    dto.setTag(jpa.getTag());
    return dto;
  }

  private Rule toJpa(Plan plan, RuleDto dto) {
    Rule jpa = new Rule();
    jpa.setPlan(plan);
    jpa.setPredicate(dto.getPredicate());
    jpa.setQueueId(dto.getQueueId());
    jpa.setTag(dto.getTag());
    return jpa;
  }

  private List<RuleDto> fromJpaRules(List<Rule> jpaRules) {
    List<RuleDto> dtoRules = new ArrayList<>();
    jpaRules.stream().forEach(jpa -> dtoRules.add(fromJpa(jpa)));
    return dtoRules;
  }

  public List<Rule> toJpaRules(Plan plan, List<RuleDto> dtoRules) {
    List<Rule> jpaRules = new ArrayList<>();
    dtoRules.stream().forEach(dto -> jpaRules.add(toJpa(plan, dto)));
    return jpaRules;
  }

}
