/* 
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

  public RouterObjectId(String id, RouterObjectId sameRouterObjectId) {
    super(id);
    setRouterId(sameRouterObjectId.getRouterId());
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
