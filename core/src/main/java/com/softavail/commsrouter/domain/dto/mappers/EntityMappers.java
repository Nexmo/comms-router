/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;

/**
 *
 * @author ikrustev
 */
public class EntityMappers {

  public final AgentMapper agent;
  public final AttributesMapper attributes;
  public final PlanMapper plan;
  public final QueueMapper queue;
  public final RouterMapper router;
  public final TaskMapper task;

  public EntityMappers() {
    attributes = new AttributesMapper();
    agent = new AgentMapper(attributes);
    plan = new PlanMapper();
    queue = new QueueMapper();
    router = new RouterMapper();
    task = new TaskMapper(attributes);
  }

}
