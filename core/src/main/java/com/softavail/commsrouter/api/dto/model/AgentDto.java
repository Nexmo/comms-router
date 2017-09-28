/*
 * To change this license header, choose License Headers in Project AttributeGroupDto. To change
 * this template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model;

import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;

import java.util.List;

/**
 *
 * @author ikrustev
 */
public class AgentDto extends RouterObjectId {

  private AttributeGroupDto capabilities;
  private String address;
  private AgentState state;
  private List<String> queueIds;

  public AgentDto() {}

  public AttributeGroupDto getCapabilities() {
    return capabilities;
  }

  public void setCapabilities(AttributeGroupDto capabilities) {
    this.capabilities = capabilities;
  }

  public String getAddress() {
    return address;
  }

  public void setAddress(String address) {
    this.address = address;
  }

  public AgentState getState() {
    return state;
  }

  public void setState(AgentState state) {
    this.state = state;
  }

  public List<String> getQueueIds() {
    return queueIds;
  }

  public void setQueueIds(List<String> queueIds) {
    this.queueIds = queueIds;
  }

}
