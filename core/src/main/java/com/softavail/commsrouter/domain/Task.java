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
import com.softavail.commsrouter.api.dto.model.TaskState;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.util.Date;
import java.util.concurrent.TimeUnit;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

/**
 * @author ikrustev
 */
@Entity
@Table(name = "task")
public class Task extends RouterObject {

  @OneToOne(cascade = CascadeType.ALL)
  @JoinColumn(name = "requirements_attribute_group_id")
  private AttributeGroup requirements;

  @OneToOne(cascade = CascadeType.ALL)
  @JoinColumn(name = "user_context_attribute_group_id")
  private AttributeGroup userContext;

  @Enumerated(EnumType.STRING)
  private TaskState state;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "rule_id")
  private Rule rule;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "queue_id")
  private Queue queue;

  @ManyToOne(fetch = FetchType.LAZY)
  private Agent agent;

  @Column(name = "callback_url")
  private String callbackUrl;

  @Column(name = "priority", nullable = false)
  private Long priority = 0L;

  @CreationTimestamp
  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "create_date")
  private Date createDate;

  @UpdateTimestamp
  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "update_date")
  private Date updateDate;

  @Column(name = "timeout", nullable = false)
  private Long queuedTimeout = TimeUnit.HOURS.toSeconds(1); // default 1h - in seconds

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "current_route")
  private Route currentRoute;

  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "expiration_date")
  private Date expirationDate;

  @Column(name = "tag")
  private String tag;

  public Task() {}

  public Task(RouterObjectRef objectId) {
    super(objectId.getRef());
  }

  public AttributeGroup getRequirements() {
    return requirements;
  }

  public void setRequirements(AttributeGroup requirements) {
    this.requirements = requirements;
  }

  public AttributeGroup getUserContext() {
    return userContext;
  }

  public void setUserContext(AttributeGroup userContext) {
    this.userContext = userContext;
  }

  public TaskState getState() {
    return state;
  }

  public void setState(TaskState state) {
    this.state = state;
  }

  public String getCallbackUrl() {
    return callbackUrl;
  }

  public void setCallbackUrl(String callbackUrl) {
    this.callbackUrl = callbackUrl;
  }

  public Rule getRule() {
    return rule;
  }

  public void setRule(Rule rule) {
    this.rule = rule;
  }

  public Queue getQueue() {
    return queue;
  }

  public void setQueue(Queue queue) {
    this.queue = queue;
  }

  public Agent getAgent() {
    return agent;
  }

  public void setAgent(Agent agent) {
    this.agent = agent;
  }

  public Long getPriority() {
    return priority;
  }

  public void setPriority(Long priority) {
    if (priority != null) {
      this.priority = priority;
    }
  }

  public Date getCreateDate() {
    return createDate;
  }

  public Date getUpdateDate() {
    return updateDate;
  }

  public Long getQueuedTimeout() {
    return queuedTimeout;
  }

  public void setQueuedTimeout(Long queuedTimeout) {
    if (queuedTimeout != null) {
      this.queuedTimeout = queuedTimeout;
    }
  }

  public Route getCurrentRoute() {
    return currentRoute;
  }

  public void setCurrentRoute(Route currentRoute) {
    this.currentRoute = currentRoute;
  }

  public Date getExpirationDate() {
    return expirationDate;
  }

  public void setExpirationDate(Date expirationDate) {
    this.expirationDate = expirationDate;
  }

  public String getTag() {
    return tag;
  }

  public void setTag(String tag) {
    this.tag = tag;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder().append("JpaTask [")
        .append("requirements = ").append(requirements)
        .append(", userContext = ").append(userContext)
        .append(", callbackUrl = ").append(callbackUrl)
        .append("]");
    return sb.toString();
  }

  public void makeCompleted() {
    makeFinal(TaskState.completed);
  }

  public void makeCanceled() {
    makeFinal(TaskState.canceled);
  }

  private void makeFinal(TaskState state) {
    assert state.isFinal();
    setState(state);
    setQueue(null);
    setAgent(null);
    setRule(null);
    setCurrentRoute(null);
  }

}
