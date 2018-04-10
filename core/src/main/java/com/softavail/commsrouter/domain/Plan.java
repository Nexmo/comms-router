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

package com.softavail.commsrouter.domain;

import com.softavail.commsrouter.api.dto.model.RouterObjectRef;

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

  private Integer revision;

  public Plan() {}

  public Plan(RouterObjectRef objectId) {
    super(objectId.getRef());
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
  }

  public void markBackup(String newDescription) {
    String rev = String.valueOf(revision);
    setRef(getRef() + "_" + rev);

    // if no new description change the old for easier distinction
    if (newDescription == null && getDescription() != null) {
      setDescription("Backup " + rev + " of " + getDescription());
    }
  }

  public Integer getRevision() {
    return revision;
  }

  public void setRevision(Integer revision) {
    this.revision = revision;
  }

}
