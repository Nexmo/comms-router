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

import com.softavail.commsrouter.api.dto.model.RouterObjectRef;

import java.util.ArrayList;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "queue")
public class Queue extends RouterObject {

  private String description;
  private String predicate;

  @OneToMany(mappedBy = "queue", cascade = CascadeType.ALL, orphanRemoval = true)
  private List<AgentQueueMapping> agentQueueMappings = new ArrayList<>();

  public Queue() {}

  public Queue(RouterObjectRef objectRef) {
    super(objectRef.getRef());
  }

  public String getPredicate() {
    return predicate;
  }

  public void setPredicate(String predicate) {
    this.predicate = predicate;
  }

  public List<AgentQueueMapping> getAgentQueueMappings() {
    return agentQueueMappings;
  }

  public void setAgentQueueMappings(List<AgentQueueMapping> agentQueueMappings) {
    this.agentQueueMappings = agentQueueMappings;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

}
