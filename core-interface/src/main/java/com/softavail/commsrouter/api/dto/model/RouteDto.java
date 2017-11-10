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

import java.io.Serializable;

/**
 *
 * @author Ergyun Syuleyman
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
