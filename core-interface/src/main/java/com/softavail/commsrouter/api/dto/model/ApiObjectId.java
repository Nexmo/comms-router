package com.softavail.commsrouter.api.dto.model;

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
  public String toString() {
    final StringBuilder sb = new StringBuilder("ApiObjectId{");
    sb.append("id='").append(id).append('\'');
    sb.append('}');
    return sb.toString();
  }

}
