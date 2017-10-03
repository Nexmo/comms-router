/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain;

import java.io.Serializable;
import java.util.Objects;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "rule")
public class Rule implements Serializable {

  @Id
  @GeneratedValue
  private Long id;

  private String tag;
  private String predicate;

  @Column(name = "queue_id")
  private String queueId;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "plan_id")
  private Plan plan;

  @Column(name = "queued_task_timeout", nullable = false)
  private Long queuedTaskTimeout = new Long(24 * 60 * 60); // default 24h - in seconds

  @Override
  public boolean equals(Object rhs) {
    if (this == rhs) {
      return true;
    }
    if (!(rhs instanceof Rule)) {
      return false;
    }
    return Objects.equals(id, ((Rule) rhs).id);
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

  public String getTag() {
    return tag;
  }

  public void setTag(String tag) {
    this.tag = tag;
  }

  public String getPredicate() {
    return predicate;
  }

  public void setPredicate(String predicate) {
    this.predicate = predicate;
  }

  public String getQueueId() {
    return queueId;
  }

  public void setQueueId(String queueId) {
    this.queueId = queueId;
  }

  public Plan getPlan() {
    return plan;
  }

  public void setPlan(Plan plan) {
    this.plan = plan;
  }

  public Long getQueuedTaskTimeout() {
    return queuedTaskTimeout;
  }

  public void setQueuedTaskTimeout(Long queuedTaskTimeout) {
    if (queuedTaskTimeout != null) {
      this.queuedTaskTimeout = queuedTaskTimeout;
    }
  }

}
