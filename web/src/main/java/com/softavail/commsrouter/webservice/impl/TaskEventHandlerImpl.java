package com.softavail.commsrouter.webservice.impl;

import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class TaskEventHandlerImpl {

  private final Client client;
  private TaskDto task;
  private AgentDto agent;

  public TaskEventHandlerImpl(Client client) {
    this.client = client;
  }

  public void setTask(TaskDto task) {
    this.task = task;
  }

  public void setAgent(AgentDto agent) {
    this.agent = agent;
  }

  public void handle() {
    String callbackUrl = task.getCallbackUrl();

    client.target(callbackUrl)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .post(Entity.entity(agent, MediaType.APPLICATION_JSON_TYPE));
  }

}
