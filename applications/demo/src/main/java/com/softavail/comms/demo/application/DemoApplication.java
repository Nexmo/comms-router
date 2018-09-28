package com.softavail.comms.demo.application;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.glassfish.jersey.logging.LoggingFeature;
import org.glassfish.jersey.server.ResourceConfig;

import javax.ws.rs.ApplicationPath;

/**
 * Created by @author mapuo on 28.08.17.
 */
@ApplicationPath("/api")
public class DemoApplication extends ResourceConfig {

  private static final Logger LOGGER = LogManager.getLogger(DemoApplication.class);

  /**
   * Starting point of the application
   *
   * This class will be loaded and started by Servlet 3
   */
  public DemoApplication() {
    register(new LoggingFeature());
    register(new ApplicationBinder());
    packages(DemoApplication.class.getPackage().getName());

    LOGGER.debug("Application started!");
  }

}
