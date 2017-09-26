package com.softavail.commsrouter.api.dto.model;

import java.util.Objects;

public class ApiObjectId {

  private String id;
  
  public ApiObjectId() {}
  
  public ApiObjectId(String id) {
    setId(id);
  }

  public ApiObjectId(ApiObjectId rhs) {
    setId(rhs.getId());
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  @Override
  public boolean equals(Object object) {
    if (this == object) {
      return true;
    }
    if (object == null || getClass() != object.getClass()) {
      return false;
    }
    ApiObjectId apiObjectId = (ApiObjectId) object;
    return Objects.equals(getId(), apiObjectId.getId());
  }

  @Override
  public int hashCode() {
    return Objects.hash(getId(), getClass());
  }

}
