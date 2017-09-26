/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.domain.Plan;
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
    return dto;
  }

  private RuleDto toDto(Rule jpa) {
    RuleDto dto = new RuleDto();
    dto.setPredicate(jpa.getPredicate());
    dto.setQueueId(jpa.getQueueId());
    dto.setTag(jpa.getTag());
    return dto;
  }

  private Rule fromDto(RuleDto dto) {
    Rule rule = new Rule();
    rule.setPredicate(dto.getPredicate());
    rule.setQueueId(dto.getQueueId());
    rule.setTag(dto.getTag());
    return rule;
  }

  private List<RuleDto> toDtoRules(List<Rule> jpaRules) {
    List<RuleDto> dtoRules = new ArrayList<>();
    jpaRules.stream().forEach(jpa -> dtoRules.add(toDto(jpa)));
    return dtoRules;
  }

  public void addDtoRules(Plan plan, List<RuleDto> dtoRules) {
    if (dtoRules == null) {
      return;
    }
    dtoRules.stream().forEach(dto -> plan.addRule(fromDto(dto)));
  }

}
