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
}
