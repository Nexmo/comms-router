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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OrderColumn;
import javax.persistence.Table;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "rule")
public class Rule implements Serializable {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private String tag;
  private String predicate;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "plan_id")
  private Plan plan;

  @OneToMany(mappedBy = "rule", cascade = CascadeType.ALL, orphanRemoval = true)
  @OrderColumn(name = "route_order")
  private List<Route> routes = new ArrayList<>();

  @Override
  public boolean equals(Object rhs) {
    if (this == rhs) {
      return true;
    }
    if (!(rhs instanceof Rule)) {
      return false;
    }
    return Objects.equals(id, ((Rule) rhs).id);
  }

  @Override
  public int hashCode() {
    return id == null ? 0 : id.hashCode();
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getTag() {
    return tag;
  }

  public void setTag(String tag) {
    this.tag = tag;
  }

  public String getPredicate() {
    return predicate;
  }

  public void setPredicate(String predicate) {
    this.predicate = predicate;
  }

  public Plan getPlan() {
    return plan;
  }

  public void setPlan(Plan plan) {
    this.plan = plan;
  }

  public List<Route> getRoutes() {
    return routes;
  }

  public void setRoutes(List<Route> routes) {
    this.routes = routes;
  }

  public void addRoute(Route route) {
    route.setRule(this);
    routes.add(route);
  }

  public void removeRoute(Route route) {
    routes.remove(route);
    route.setRule(null);
  }

  public void removeRoutes() {
    routes.stream().forEach(route -> route.setRule(null));
    routes.clear();
  }
}
