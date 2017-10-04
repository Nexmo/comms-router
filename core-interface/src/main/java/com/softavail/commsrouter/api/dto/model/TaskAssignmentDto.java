/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model;

/**
 *
 * @author ikrustev
 */
public class TaskAssignmentDto {

  private TaskDto task;
  private AgentDto agent;

  public TaskAssignmentDto() {}

  public TaskAssignmentDto(TaskDto task, AgentDto agent) {
    this.task = task;
    this.agent = agent;
  }

  public TaskDto getTask() {
    return task;
  }

  public void setTask(TaskDto task) {
    this.task = task;
  }

  public AgentDto getAgent() {
    return agent;
  }

  public void setAgent(AgentDto agent) {
    this.agent = agent;
  }

}
