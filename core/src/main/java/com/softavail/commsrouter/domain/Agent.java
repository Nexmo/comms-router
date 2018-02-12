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

package com.softavail.commsrouter.domain;

import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

/**
 * @author ikrustev
 */
@Entity
@Table(name = "agent")
public class Agent extends RouterObject {

  @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
  @JoinColumn(name = "capabilities_attribute_group_id")
  private AttributeGroup capabilities;

  private String name;

  private String description;

  private String address;

  @Enumerated(EnumType.STRING)
  private AgentState state;

  @OneToMany(mappedBy = "agent", cascade = CascadeType.ALL, orphanRemoval = true)
  private List<AgentQueueMapping> agentQueueMappings = new ArrayList<>();

  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "last_time_at_busy_state")
  private Date lastTimeAtBusyState;

  public Agent() {}

  public Agent(RouterObjectRef objectRef) {
    super(objectRef.getRef());
  }

  public AttributeGroup getCapabilities() {
    return capabilities;
  }

  public void setCapabilities(AttributeGroup capabilities) {
    this.capabilities = capabilities;
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
    if (state == AgentState.busy
        || state == AgentState.ready) {
      updateLastTimeAtBusyState();
    }
  }

  public List<AgentQueueMapping> getAgentQueueMappings() {
    return agentQueueMappings;
  }

  public void setAgentQueueMappings(List<AgentQueueMapping> agentQueueMappings) {
    this.agentQueueMappings = agentQueueMappings;
  }

  public Date getLastTimeAtBusyState() {
    return lastTimeAtBusyState;
  }

  public void updateLastTimeAtBusyState() {
    this.lastTimeAtBusyState = new Date();
  }

}
