package com.softavail.comms.demo.application.factory;

import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.commsrouter.api.interfaces.TaskService;
import com.softavail.commsrouter.client.TaskServiceClient;
import org.glassfish.hk2.api.Factory;

import javax.inject.Inject;
import javax.ws.rs.client.Client;

/**
 * Created by @author mapuo on 04.10.17.
 */
public class TaskServiceClientFactory implements Factory<TaskService> {

  @Inject
  Client client;

  @Inject
  Configuration configuration;

  @Override
  public TaskService provide() {
    return new TaskServiceClient(
        client, configuration.getCommsApiEndpoint(), configuration.getCommsRouterId());
  }

  @Override
  public void dispose(TaskService instance) {
    // Do nothing
  }

}
