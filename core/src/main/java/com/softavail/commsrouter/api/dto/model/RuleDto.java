/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model;

import java.io.Serializable;

/**
 *
 * @author ikrustev
 */
public class RuleDto implements Serializable {

  private String tag;
  private String predicate;
  private String queueId;
  private Long queuedTaskTimeout;

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

  public Long getQueuedTaskTimeout() {
    return queuedTaskTimeout;
  }

  public void setQueuedTaskTimeout(Long queuedTaskTimeout) {
    this.queuedTaskTimeout = queuedTaskTimeout;
  }

}
