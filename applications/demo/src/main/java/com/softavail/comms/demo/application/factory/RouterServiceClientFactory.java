package com.softavail.comms.demo.application.factory;

import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.commsrouter.api.interfaces.RouterService;
import com.softavail.commsrouter.client.RouterServiceClient;
import org.glassfish.hk2.api.Factory;

import javax.inject.Inject;
import javax.ws.rs.client.Client;

/**
 * Created by @author mapuo on 04.10.17.
 */
public class RouterServiceClientFactory implements Factory<RouterService> {

  @Inject
  Client client;

  @Inject
  Configuration configuration;

  @Override
  public RouterService provide() {
    return new RouterServiceClient(client, configuration.getCommsApiEndpoint());
  }

  @Override
  public void dispose(RouterService instance) {
    // Do nothing
  }

}
