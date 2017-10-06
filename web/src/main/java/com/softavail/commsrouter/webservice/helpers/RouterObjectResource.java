package com.softavail.commsrouter.webservice.helpers;

import com.softavail.commsrouter.api.dto.model.RouterObjectId;

import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 31.08.17.
 */
public class RouterObjectResource {

  protected String routerId;
  protected UriBuilder entryPoint;

  public void setRouterId(final String routerId) {
    this.routerId = routerId;
  }

  public void setEntryPoint(UriBuilder entryPoint) {
    this.entryPoint = entryPoint;
  }

  protected RouterObjectId getRouterObjectId(String id) {
    return new RouterObjectId.Builder()
        .setRouterId(routerId)
        .setId(id)
        .build();
  }

}
