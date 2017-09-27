package com.softavail.commsrouter.api.dto.model;

public class RouterObjectId extends ApiObjectId {
  
  private String routerId;

  public RouterObjectId() {}

  public RouterObjectId(RouterObjectId rhs) {
    super(rhs);
    setRouterId(rhs.routerId);
  }

  public RouterObjectId(String id, String routerId) {
    super(id);
    setRouterId(routerId);
  }

  public String getRouterId() {
    return routerId;
  }

  public void setRouterId(String routerId) {
    this.routerId = routerId;
  }

  @Override
  public String toString() {
    return "" + getRouterId() + ":" + getId();
  }

  public static class Builder {

    private String id;
    private String routerId;

    public Builder() {}

    public Builder setId(String id) {
      this.id = id;
      return this;
    }

    public Builder setRouterId(String routerId) {
      this.routerId = routerId;
      return this;
    }

    public RouterObjectId build() {
      return new RouterObjectId(id, routerId);
    }

  }

  public static Builder builder() {
    return new Builder();
  }
}
