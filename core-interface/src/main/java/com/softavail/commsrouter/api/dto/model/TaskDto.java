/*
 * To change this license header, choose License Headers in Project AttributeGroupDto. To change
 * this template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;

/**
 *
 * @author ikrustev
 */
public class TaskDto extends RouterObjectId {

  private AttributeGroupDto requirements;
  private AttributeGroupDto userContext;
  private TaskState state;
  private String planId;
  private String queueId;
  private String agentId;
  private String callbackUrl;
  private Long priority;

  public TaskDto() {}

  public TaskDto(CreateTaskArg createArg, RouterObjectId objectId) {
    super(objectId.getId(), objectId.getRouterId());
    requirements = createArg.getRequirements();
    userContext = createArg.getUserContext();
    planId = createArg.getPlanId();
    queueId = createArg.getQueueId();
    callbackUrl = createArg.getCallbackUrl().toString();
    priority = createArg.getPriority();
  }

  public TaskDto(TaskDto taskDto) {
    super(taskDto);
    requirements = taskDto.requirements;
    userContext = taskDto.userContext;
    planId = taskDto.planId;
    queueId = taskDto.queueId;
    agentId = taskDto.agentId;
    callbackUrl = taskDto.callbackUrl;
    priority = taskDto.priority;
  }

  public AttributeGroupDto getRequirements() {
    return requirements;
  }

  public void setRequirements(AttributeGroupDto requirements) {
    this.requirements = requirements;
  }

  public AttributeGroupDto getUserContext() {
    return userContext;
  }

  public void setUserContext(AttributeGroupDto userContext) {
    this.userContext = userContext;
  }

  public TaskState getState() {
    return state;
  }

  public void setState(TaskState state) {
    this.state = state;
  }

  public String getPlanId() {
    return planId;
  }

  public void setPlanId(String planId) {
    this.planId = planId;
  }

  public String getQueueId() {
    return queueId;
  }

  public void setQueueId(String queueId) {
    this.queueId = queueId;
  }

  public String getAgentId() {
    return agentId;
  }

  public void setAgentId(String agentId) {
    this.agentId = agentId;
  }

  public String getCallbackUrl() {
    return callbackUrl;
  }

  public void setCallbackUrl(String callbackUrl) {
    this.callbackUrl = callbackUrl;
  }

  public Long getPriority() {
    return priority;
  }

  public void setPriority(Long priority) {
    this.priority = priority;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder()
        .append("Task [")
        .append("requirements = ").append(getRequirements())
        .append(", userContext = ").append(getUserContext())
        .append(", callbackUrl = ").append(getCallbackUrl())
        .append("]");
    return sb.toString();
  }

}
