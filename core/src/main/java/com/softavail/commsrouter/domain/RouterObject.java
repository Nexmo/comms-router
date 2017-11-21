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

package com.softavail.commsrouter.domain;

import java.util.Objects;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.MappedSuperclass;

/**
 *
 * @author ikrustev
 */
@MappedSuperclass
public class RouterObject extends ApiObject {

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "router_id")
  private Router router;

  public RouterObject() {}

  public RouterObject(RouterObject rhs) {
    super(rhs);
    this.router = rhs.router;
  }

  public RouterObject(String id) {
    super(id);
  }

  public Router getRouter() {
    return router;
  }

  public void setRouter(Router router) {
    this.router = router;
  }

  @Override
  public String toString() {
    return getRouter().getRef() + ":" + getRef();
  }

  @Override
  public boolean equals(Object object) {
    boolean equals = super.equals(object);
    if (equals) {
      RouterObject routerObject = (RouterObject) object;
      return Objects.equals(getRouter(), routerObject.getRouter());
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(getRef(), getRouter(), getVersion(), getClass());
  }

}
