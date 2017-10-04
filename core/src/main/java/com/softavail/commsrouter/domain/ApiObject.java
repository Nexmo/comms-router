/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain;

import java.io.Serializable;
import java.util.Objects;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;
import javax.persistence.Version;

@MappedSuperclass
public class ApiObject implements Serializable {

  @Id
  private String id;

  @Version
  private Integer version;

  public ApiObject() {}

  public ApiObject(ApiObject rhs) {
    this.id = rhs.id;
  }

  public ApiObject(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public Integer getVersion() {
    return version;
  }

  public void setVersion(Integer version) {
    this.version = version;
  }

  @Override
  public boolean equals(Object object) {
    if (this == object) {
      return true;
    }
    if (object == null || getClass() != object.getClass()) {
      return false;
    }
    ApiObject apiObject = (ApiObject) object;
    return Objects.equals(getId(), apiObject.getId());
  }

  @Override
  public int hashCode() {
    return Objects.hash(getId(), getVersion(), getClass());
  }

}
