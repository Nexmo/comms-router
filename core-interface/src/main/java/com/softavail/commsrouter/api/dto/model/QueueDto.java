/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;


/**
 *
 * @author ikrustev
 */
public class QueueDto extends RouterObjectId {

  private String description;
  private String predicate;

  public QueueDto() {}

  public QueueDto(CreateQueueArg queueArg, RouterObjectId objectId) {
    super(objectId.getId(), objectId.getRouterId());
    description = queueArg.getDescription();
    predicate = queueArg.getPredicate();
  }

  public String getPredicate() {
    return predicate;
  }

  public void setPredicate(String predicate) {
    this.predicate = predicate;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

}
