package com.softavail.comms.demo.application.factory;

import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.commsrouter.api.interfaces.QueueService;
import com.softavail.commsrouter.client.QueueServiceClient;
import org.glassfish.hk2.api.Factory;

import javax.inject.Inject;
import javax.ws.rs.client.Client;

/**
 * Created by @author mapuo on 04.10.17.
 */
public class QueueServiceClientFactory implements Factory<QueueService> {

  @Inject
  Client client;

  @Inject
  Configuration configuration;

  @Override
  public QueueService provide() {
    return new QueueServiceClient(
        client, configuration.getCommsApiEndpoint(), configuration.getCommsRouterId());
  }

  @Override
  public void dispose(QueueService instance) {
    // Do nothing
  }

}
