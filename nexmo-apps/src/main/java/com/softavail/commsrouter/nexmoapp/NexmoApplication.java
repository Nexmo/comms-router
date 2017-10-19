package com.softavail.commsrouter.nexmoapp;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.glassfish.jersey.server.ResourceConfig;

import javax.servlet.ServletContext;
import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Context;

/**
 * Created by @author mapuo on 09.10.17.
 */
@ApplicationPath("/api")
public class NexmoApplication extends ResourceConfig {

  private static final Logger LOGGER = LogManager.getLogger(NexmoApplication.class);

  public NexmoApplication(@Context ServletContext servletContext) {

    ApplicationContext applicationContext =
        (ApplicationContext) servletContext.getAttribute(WebServletListener.APP_CONTEXT);

    register(new ApplicationBindings(applicationContext));
    packages("com.softavail.commsrouter.nexmoapp.api.resources");

    LOGGER.debug("Application started!");
  }

}
