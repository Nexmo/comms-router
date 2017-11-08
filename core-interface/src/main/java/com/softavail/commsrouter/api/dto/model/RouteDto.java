/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model;

import java.io.Serializable;

/**
 *
 * @author ergyunsyuleyman
 */
public class RouteDto implements Serializable {

  private String queueId;
  private Long priority;
  private Long timeout;

  public String getQueueId() {
    return queueId;
  }

  public void setQueueId(String queueId) {
    this.queueId = queueId;
  }

  public Long getPriority() {
    return priority;
  }

  public void setPriority(Long priority) {
    this.priority = priority;
  }

  public Long getTimeout() {
    return timeout;
  }

  public void setTimeout(Long timeout) {
    this.timeout = timeout;
  }

  public static class Builder{
    private RouteDto route = new RouteDto();

    public Builder(String queueId) {
      route.setQueueId(queueId);
    }

    public Builder priority(Long priority) {
      route.setPriority(priority);
      return this;
    }

    public Builder timeout(Long timeout) {
      route.setTimeout(timeout);
      return this;
    }

    public RouteDto build() {
      return route;
    }

  }

}
