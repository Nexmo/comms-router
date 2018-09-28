package com.softavail.comms.demo.application.factory;

import org.glassfish.hk2.api.Factory;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.logging.LoggingFeature;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;

/**
 * Created by @author mapuo on 30.08.17.
 */
public class ClientFactory implements Factory<Client> {

  private Client client;

  public ClientFactory() {
    ClientConfig clientConfig = new ClientConfig();
    // clientConfig.property(ClientProperties.CONNECT_TIMEOUT, 60000);
    // clientConfig.property(ClientProperties.READ_TIMEOUT, 10000);
    clientConfig.register(new LoggingFeature());
    client = ClientBuilder.newClient(clientConfig);
  }

  @Override
  public Client provide() {
    return client;
  }

  @Override
  public void dispose(Client instance) {
    instance.close();
  }

}
