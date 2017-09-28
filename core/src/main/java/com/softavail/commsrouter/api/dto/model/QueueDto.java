/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.util.Fields;

import java.util.List;

/**
 *
 * @author ikrustev
 */
public class QueueDto extends RouterObjectId {

  private String description;
  private String predicate;
  private List<String> agentIds;

  public QueueDto() {}

  public QueueDto(CreateQueueArg queueArg, RouterObject objectId) {
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

  public List<String> getAgentIds() {
    return agentIds;
  }

  public void setAgentIds(List<String> agentIds) {
    this.agentIds = agentIds;
  }

  public void update(UpdateQueueArg updateArg) {
    Fields.update(this::setDescription, description, updateArg.getDescription());
    Fields.update(this::setPredicate, predicate, updateArg.getPredicate());
  }

}
