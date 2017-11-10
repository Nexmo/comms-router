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

package com.softavail.commsrouter.api.dto.arg;

import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;

import java.net.URL;

/**
 * @author ikrustev
 */
public class CreateTaskArg {

  private String queueId;
  private String planId;
  private AttributeGroupDto requirements;
  private AttributeGroupDto userContext;
  private URL callbackUrl;

  public String getQueueId() {
    return queueId;
  }

  public void setQueueId(String queueId) {
    this.queueId = queueId;
  }

  public String getPlanId() {
    return planId;
  }

  public void setPlanId(String planId) {
    this.planId = planId;
  }

  public AttributeGroupDto getRequirements() {
    return requirements;
  }

  public void setRequirements(AttributeGroupDto requirements) {
    this.requirements = requirements;
  }

  public AttributeGroupDto getUserContext() {
    return userContext;
  }

  public void setUserContext(AttributeGroupDto userContext) {
    this.userContext = userContext;
  }

  public URL getCallbackUrl() {
    return callbackUrl;
  }

  public void setCallbackUrl(URL callbackUrl) {
    this.callbackUrl = callbackUrl;
  }

  public static class Builder {
    private CreateTaskArg arg = new CreateTaskArg();

    public Builder() {
    }

    public Builder callback(URL url) {
      arg.setCallbackUrl(url);
      return this;
    }

    public Builder requirements(AttributeGroupDto requirements) {
      arg.setRequirements(requirements);
      return this;
    }

    public CreateTaskArg build() {
      return arg;
    }

  }

}
