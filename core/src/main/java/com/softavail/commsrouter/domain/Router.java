/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain;

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.model.ApiObject;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "router")
public class Router extends ApiObject {

  private String name;
  private String description;

  public Router() {}

  public Router(CreateRouterArg createArg) {
    super(createArg);
    this.name = createArg.getName();
    this.description = createArg.getDescription();
  }

  public Router(Router jpa) {
    super(jpa);
    this.name = jpa.getName();
    this.description = jpa.getDescription();
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  @Override
  public String toString() {
    return "JpaRouter: [" + "id=" + getId() + "]";
  }

}
