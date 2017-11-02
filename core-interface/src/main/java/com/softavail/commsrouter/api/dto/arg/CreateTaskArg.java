/*
 * To change this license header, choose License Headers in Project AttributeGroupDto. To change
 * this template file, choose Tools | Templates and open the template in the editor.
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
  private String tag;

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

    public Builder requirements(AttributeGroupDto requirements) {
      arg.setRequirements(requirements);
      return this;
    }

    public CreateTaskArg build() {
      return arg;
    }

  }

}
