/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;

import java.util.ArrayList;
import java.util.List;


/**
 *
 * @author ikrustev
 */
public class PlanDto extends RouterObject {

  private String description;
  private List<RuleDto> rules = new ArrayList<>();

  public PlanDto() {}

  public PlanDto(CreatePlanArg createArg, RouterObjectId objectId) {
    super(objectId.getId(), objectId.getRouterId());
    this.description = createArg.getDescription();
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public List<RuleDto> getRules() {
    return rules;
  }

  public void setRules(List<RuleDto> rules) {
    this.rules = rules;
  }

}
