package com.softavail.commsrouter.client;

import javax.ws.rs.client.Client;

/**
 * Created by @author mapuo on 12.10.17.
 */
public class CommsRouterClient {

  private final Client client;
  private final String endpoint;
  private final String routerId;

  public CommsRouterClient(final Client client, final String endpoint, final String routerId) {
    this.client = client;
    this.endpoint = endpoint;
    this.routerId = routerId;
  }

  public TaskServiceClient getTaskClient() {
    return new TaskServiceClient(client, endpoint, routerId);
  }

  public AgentServiceClient getAgentClient() {
    return new AgentServiceClient(client, endpoint, routerId);
  }

  public PlanServiceClient getPlanClient() {
    return new PlanServiceClient(client, endpoint, routerId);
  }

  public QueueServiceClient getQueueClient() {
    return new QueueServiceClient(client, endpoint, routerId);
  }

  public RouterServiceClient getRouterClient() {
    return new RouterServiceClient(client, endpoint);
  }

}
