package com.softavail.comms.demo.application.factory;

import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.commsrouter.api.interfaces.AgentService;
import com.softavail.commsrouter.client.AgentServiceClient;
import org.glassfish.hk2.api.Factory;

import javax.inject.Inject;
import javax.ws.rs.client.Client;

/**
 * Created by @author mapuo on 04.10.17.
 */
public class AgentServiceClientFactory implements Factory<AgentService> {

  @Inject
  Client client;

  @Inject
  Configuration configuration;

  @Override
  public AgentService provide() {
    return new AgentServiceClient(
        client, configuration.getCommsApiEndpoint(), configuration.getCommsRouterId());
  }

  @Override
  public void dispose(AgentService instance) {
    // Do nothing
  }

}
