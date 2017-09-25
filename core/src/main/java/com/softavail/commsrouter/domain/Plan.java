/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.model.RouterObject;

import java.util.ArrayList;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "plan")
public class Plan extends RouterObject {

  private String description;

  @OneToMany(mappedBy = "plan", cascade = CascadeType.ALL, orphanRemoval = true)
  private List<Rule> rules = new ArrayList<>();

  public Plan() {}

  public Plan(CreatePlanArg createArg) {
    super(createArg);
    this.description = createArg.getDescription();
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public List<Rule> getRules() {
    return rules;
  }

  public void setRules(List<Rule> rules) {
    this.rules = rules;
  }

  public void addRule(Rule rule) {
    rule.setPlan(this);
    rules.add(rule);
  }

  public void removeRule(Rule rule) {
    rules.remove(rule);
    rule.setPlan(null);
  }

}
