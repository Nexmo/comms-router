/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;

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

  public Router(ApiObjectId objectId) {
    super(objectId.getId());
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
    return "Router: [" + "id=" + getId() + "]";
  }

}
