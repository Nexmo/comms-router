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

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonProperty.Access;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;

import java.util.Date;
import java.util.List;

/**
 *
 * @author ikrustev
 */
public class AgentDto extends RouterObjectRef {

  private AttributeGroupDto capabilities;
  private String address;
  private String name;
  private String description;
  private AgentState state;
  private List<String> queueRefs;
  @JsonIgnore
  private Date lastTimeAtBusyState;

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

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public AgentState getState() {
    return state;
  }

  public void setState(AgentState state) {
    this.state = state;
  }

  public List<String> getQueueRefs() {
    return queueRefs;
  }

  public void setQueueRefs(List<String> queueRefs) {
    this.queueRefs = queueRefs;
  }

  @JsonProperty(access = Access.READ_ONLY)
  public Date getLastTimeAtBusyState() {
    return lastTimeAtBusyState;
  }

  @JsonIgnore
  public void setLastTimeAtBusyState(Date lastTimeAtBusyState) {
    this.lastTimeAtBusyState = lastTimeAtBusyState;
  }

}
