/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model;

/**
 *
 * @author ikrustev
 */
public class RouterDto extends ApiObject {

  private String name;
  private String description;

  public RouterDto() {}

  public RouterDto(RouterDto jpa) {
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
    return "Router: [" + "id=" + getId() + "]";
  }

}
