/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain;

import java.io.Serializable;
import java.util.Objects;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;

/**
 *
 * @author ergyunsyuleyman
 */
@Entity
@Table(name = "route")
public class Route implements Serializable {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "queue_id")
  private String queueId;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "rule_id")
  private Rule rule;

  @Column(name = "task_priority")
  private Long priority;

  @Column(name = "queued_task_timeout")
  private Long timeout;

  @OneToOne(cascade = CascadeType.ALL)
  @JoinColumn(name = "plan_id")
  private Plan plan;

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

  public String getQueueId() {
    return queueId;
  }

  public void setQueueId(String queueId) {
    this.queueId = queueId;
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

  public Plan getPlan() {
    return plan;
  }

  public void setPlan(Plan plan) {
    this.plan = plan;
  }

}
