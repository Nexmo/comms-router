/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain;

import com.softavail.commsrouter.api.dto.model.RouterObjectId;

import java.util.ArrayList;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.OrderColumn;
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
  @OrderColumn(name = "rule_order")
  private List<Rule> rules = new ArrayList<>();

  @OneToOne(cascade = CascadeType.ALL)
  @JoinColumn(name = "default_route")
  private Route defaultRoute;

  public Plan() {}

  public Plan(RouterObjectId objectId) {
    super(objectId.getId());
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

  public void removeRules() {
    rules.stream().forEach(rule -> rule.setPlan(null));
    rules.clear();
  }

  public Route getDefaultRoute() {
    return defaultRoute;
  }

  public void setDefaultRoute(Route defaultRoute) {
    this.defaultRoute = defaultRoute;
    this.defaultRoute.setPlan(this);
  }

}
