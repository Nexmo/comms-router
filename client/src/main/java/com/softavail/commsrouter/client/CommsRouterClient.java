package com.softavail.commsrouter.client;

import javax.ws.rs.client.Client;

/**
 * Created by @author mapuo on 12.10.17.
 */
public class CommsRouterClient {

  private final Client client;
  private final String endpoint;

  public CommsRouterClient(final Client client, final String endpoint) {
    this.client = client;
    this.endpoint = endpoint;
  }

  public TaskServiceClient getTaskService(String routerId) {
    return new TaskServiceClient(client, endpoint, routerId);
  }

}
