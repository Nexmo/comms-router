package com.softavail.commsrouter.nexmoapp;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.glassfish.jersey.server.ResourceConfig;

import javax.ws.rs.ApplicationPath;

/**
 * Created by @author mapuo on 09.10.17.
 */
@ApplicationPath("/api")
public class NexmoApplication extends ResourceConfig {

  private static final Logger LOGGER = LogManager.getLogger(NexmoApplication.class);

  public NexmoApplication() {

    register(new ApplicationBindings());
    packages("com.softavail.commsrouter.nexmoapp.api.resources");

    LOGGER.debug("Application started!");
  }

}
