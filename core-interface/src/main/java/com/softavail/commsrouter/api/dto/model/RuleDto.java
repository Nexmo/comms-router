/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author ikrustev
 */
public class RuleDto implements Serializable {

  private String tag;
  private String predicate;
  private List<RouteDto> routes = new ArrayList<>();

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

  public List<RouteDto> getRoutes() {
    return routes;
  }

  public void setRoutes(List<RouteDto> routes) {
    this.routes = routes;
  }

}
