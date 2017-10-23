/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
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
