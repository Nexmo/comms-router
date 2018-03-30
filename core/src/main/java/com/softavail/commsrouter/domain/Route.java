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
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 *
 * @author Ergyun Syuleyman
 */
@Entity
@Table(name = "route")
public class Route implements Serializable {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "queue_id")
  private Queue queue;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "rule_id")
  private Rule rule;

  @Column(name = "task_priority")
  private Long priority;

  @Column(name = "queued_task_timeout")
  private Long timeout;

  @Override
  public boolean equals(Object rhs) {
    if (this == rhs) {
      return true;
    }
    if (!(rhs instanceof Route)) {
      return false;
    }
    return Objects.equals(id, ((Route) rhs).id);
  }

  @Override
  public int hashCode() {
    return id == null ? 0 : id.hashCode();
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public Queue getQueue() {
    return queue;
  }

  public void setQueue(Queue queue) {
    this.queue = queue;
  }

  public Rule getRule() {
    return rule;
  }

  public void setRule(Rule rule) {
    this.rule = rule;
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

}
