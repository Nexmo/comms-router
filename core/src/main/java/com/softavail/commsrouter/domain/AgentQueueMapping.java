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

import java.io.Serializable;
import java.util.Objects;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 *
 * @author ikrustev
 */
@Entity
@IdClass(AgentQueueMappingId.class)
@Table(name = "agent_queue")
public class AgentQueueMapping implements Serializable {

  private static final long serialVersionUID = 1L;

  @Id
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "agent_id")
  private Agent agent;

  @Id
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "queue_id")
  private Queue queue;

  public AgentQueueMapping() {
  }

  public AgentQueueMapping(Agent agent, Queue queue) {
    this.agent = agent;
    this.queue = queue;
  }

  public Agent getAgent() {
    return agent;
  }

  public void setAgent(Agent agent) {
    this.agent = agent;
  }

  public Queue getQueue() {
    return queue;
  }

  public void setQueue(Queue queue) {
    this.queue = queue;
  }

  @Override
  public int hashCode() {
    return Objects.hash(agent.getId(), queue.getId());
  }

  @Override
  public boolean equals(Object object) {
    if (this == object) {
      return true;
    }
    if (object == null || object.getClass() != this.getClass()) {
      return false;
    }
    AgentQueueMapping rhs = (AgentQueueMapping)object;
    return this.agent.getId().equals(rhs.agent.getId())
            && this.queue.getId().equals(rhs.queue.getId());
  }

}
