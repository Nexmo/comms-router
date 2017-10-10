package com.softavail.commsrouter.providers;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.glassfish.hk2.api.Factory;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.logging.LoggingFeature;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;

/**
 * Created by @author mapuo on 03.09.17.
 */
public class ClientFactory implements Factory<Client> {

  private static final Logger LOGGER = LogManager.getLogger(ClientFactory.class);

  @Override
  public Client provide() {
    ClientConfig config = new ClientConfig();
    config.property(ClientProperties.CONNECT_TIMEOUT, 1500);
    config.property(ClientProperties.READ_TIMEOUT, 1500);
    config.register(new LoggingFeature());
    Client client = ClientBuilder.newClient(config);

    LOGGER.debug(" *** Created client: {}", client);

    return client;
  }

  @Override
  public void dispose(Client instance) {
    LOGGER.debug(" *** Closing client: {}", instance);
    instance.close();
  }

}
