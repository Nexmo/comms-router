package com.softavail.commsrouter.webservice.impl;

import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class TaskEventHandlerImpl {

  private final Client client;
  private TaskAssignmentDto taskAssignment;

  public TaskEventHandlerImpl(Client client) {
    this.client = client;
  }

  public TaskEventHandlerImpl(Client client, TaskAssignmentDto taskAssignment) {
    this.client = client;
    this.taskAssignment = taskAssignment;
  }

  public void setTaskAssignment(TaskAssignmentDto taskAssignment) {
    this.taskAssignment = taskAssignment;
  }

  public void handle() {
    String callbackUrl = taskAssignment.getTask().getCallbackUrl();

    client.target(callbackUrl)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .post(Entity.entity(taskAssignment, MediaType.APPLICATION_JSON_TYPE));
  }

}
