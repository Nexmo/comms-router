/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
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
    return getRouter().getId() + ":" + getId();
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
    return Objects.hash(getId(), getRouter(), getVersion(), getClass());
  }

}
