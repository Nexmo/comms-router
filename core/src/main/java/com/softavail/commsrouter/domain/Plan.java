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
import com.softavail.commsrouter.util.Uuid;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.OrderColumn;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

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

  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "deleted_time")
  private Date deletedTime;

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

  public Date getDeletedTime() {
    return deletedTime;
  }

  public void setDeletedTime(Date deletedTime) {
    this.deletedTime = deletedTime;
  }

  public void markDeleted() {
    setRef(getRef() + "_" + Uuid.get());
    setDeletedTime(new Date());
  }

}
