/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;

import java.util.ArrayList;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToMany;
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

  @ManyToMany(fetch = FetchType.LAZY, mappedBy = "queues", cascade = CascadeType.REMOVE)
  private List<Agent> agents = new ArrayList<>();

  public Queue() {}

  public Queue(CreateQueueArg queueArg, RouterObjectId objectId) {
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

  public List<Agent> getAgents() {
    return agents;
  }

  public void setAgents(List<Agent> agents) {
    this.agents = agents;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

}
