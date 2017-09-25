/*
 * To change this license header, choose License Headers in Project AttributeGroupDto. To change
 * this template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.arg;

import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;

import java.net.URL;

/**
 * @author ikrustev
 */
public class CreateTaskArg extends RouterObject {

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

}
