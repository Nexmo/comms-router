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

package com.softavail.commsrouter.api.dto.model;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;

import java.util.Date;

/**
 *
 * @author ikrustev
 */
public class TaskDto extends RouterObjectRef {

  private AttributeGroupDto requirements;
  private AttributeGroupDto userContext;
  private TaskState state;
  private String queueRef;
  private String agentRef;
  private String callbackUrl;
  private Long priority;
  private Date createDate;
  private Date updateDate;
  private Long queuedTimeout;
  private String tag;

  public TaskDto() {}

  public TaskDto(CreateTaskArg createArg, RouterObjectRef objectRef) {
    super(objectRef.getRef(), objectRef.getRouterRef());
    requirements = createArg.getRequirements();
    userContext = createArg.getUserContext();
    queueRef = createArg.getQueueRef();
    callbackUrl = createArg.getCallbackUrl().toString();
  }

  public TaskDto(TaskDto taskDto) {
    super(taskDto);
    requirements = taskDto.requirements;
    userContext = taskDto.userContext;
    queueRef = taskDto.queueRef;
    agentRef = taskDto.agentRef;
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

  public String getQueueRef() {
    return queueRef;
  }

  public void setQueueRef(String queueRef) {
    this.queueRef = queueRef;
  }

  public String getAgentRef() {
    return agentRef;
  }

  public void setAgentRef(String agentRef) {
    this.agentRef = agentRef;
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

  public Date getCreateDate() {
    return createDate;
  }

  public void setCreateDate(Date createDate) {
    this.createDate = createDate;
  }

  public Date getUpdateDate() {
    return updateDate;
  }

  public void setUpdateDate(Date updateDate) {
    this.updateDate = updateDate;
  }

  public Long getQueuedTimeout() {
    return queuedTimeout;
  }

  public void setQueuedTimeout(Long queuedTimeout) {
    this.queuedTimeout = queuedTimeout;
  }

  public String getTag() {
    return tag;
  }

  public void setTag(String tag) {
    this.tag = tag;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder().append("Task [").append("requirements = ")
        .append(getRequirements()).append(", userContext = ").append(getUserContext())
        .append(", callbackUrl = ").append(getCallbackUrl()).append("]");
    return sb.toString();
  }

}
