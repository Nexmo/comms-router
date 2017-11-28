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

public class RouterObjectRef extends ApiObjectRef {

  private String routerRef;

  public RouterObjectRef() {}

  public RouterObjectRef(RouterObjectRef rhs) {
    super(rhs);
    setRouterRef(rhs.routerRef);
  }

  public RouterObjectRef(String ref, String routerRef) {
    super(ref);
    setRouterRef(routerRef);
  }

  public RouterObjectRef(String ref, RouterObjectRef sameRouterObjectRef) {
    super(ref);
    setRouterRef(sameRouterObjectRef.getRouterRef());
  }

  public String getRouterRef() {
    return routerRef;
  }

  public void setRouterRef(String routerRef) {
    this.routerRef = routerRef;
  }

  @Override
  public String toString() {
    return getRouterRef() + ":" + getRef();
  }

  public static class Builder {

    private String ref;
    private String routerRef;

    public Builder() {}

    public Builder setRef(String id) {
      this.ref = id;
      return this;
    }

    public Builder setRouterRef(String routerRef) {
      this.routerRef = routerRef;
      return this;
    }

    public RouterObjectRef build() {
      return new RouterObjectRef(ref, routerRef);
    }

  }

  public static Builder builder() {
    return new Builder();
  }
}
