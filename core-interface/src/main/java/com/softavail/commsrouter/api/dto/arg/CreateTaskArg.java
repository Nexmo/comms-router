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

  private String queueRef;
  private String planRef;
  private AttributeGroupDto requirements;
  private AttributeGroupDto userContext;
  private URL callbackUrl;
  private String tag;

  public String getQueueRef() {
    return queueRef;
  }

  public void setQueueRef(String queueRef) {
    this.queueRef = queueRef;
  }

  public String getPlanRef() {
    return planRef;
  }

  public void setPlanRef(String planRef) {
    this.planRef = planRef;
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

  public String getTag() {
    return tag;
  }

  public void setTag(String tag) {
    this.tag = tag;
  }

  public static class Builder {
    private CreateTaskArg arg = new CreateTaskArg();

    public Builder() {
    }

    public Builder callback(URL url) {
      arg.setCallbackUrl(url);
      return this;
    }

    public Builder queue(String queueId) {
      arg.setQueueRef(queueId);
      return this;
    }

    public Builder tag(String tag) {
      arg.setTag(tag);
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
