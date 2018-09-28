package com.softavail.comms.demo.application.factory;

import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.commsrouter.api.interfaces.PlanService;
import com.softavail.commsrouter.client.PlanServiceClient;
import org.glassfish.hk2.api.Factory;

import javax.inject.Inject;
import javax.ws.rs.client.Client;

/**
 * Created by @author mapuo on 04.10.17.
 */
public class PlanServiceClientFactory implements Factory<PlanService> {

  @Inject
  Client client;

  @Inject
  Configuration configuration;

  @Override
  public PlanService provide() {
    return new PlanServiceClient(
        client, configuration.getCommsApiEndpoint(), configuration.getCommsRouterId());
  }

  @Override
  public void dispose(PlanService instance) {
    // Do nothing
  }

}
