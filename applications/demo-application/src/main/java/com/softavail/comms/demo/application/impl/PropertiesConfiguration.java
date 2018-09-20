package com.softavail.comms.demo.application.impl;

import com.google.common.io.Resources;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;
import javax.inject.Singleton;

/**
 * Created by @author mapuo on 29.08.17.
 */
@Singleton
public class PropertiesConfiguration implements ConfigurationProperties {

  private static final Logger LOGGER = LogManager.getLogger(PropertiesConfiguration.class);

  private static final String APPLICATION_PROPERTIES = "application.properties";
  private static final String CALLBACK_BASE_URL = "app.callbackBaseUrl";
  private static final String NEXMO_CALLBACK_BASE_URL = "app.nexmoCallbackBaseUrl";
  private static final String APP_PHONE = "app.phone";
  private static final String APP_MUSIC_ON_HOLD_URL = "app.musicOnHoldUrl";
  private static final String NEXMO_APP_ID = "nexmo.appId";
  private static final String NEXMO_APP_PRIVATE_KEY = "nexmo.appPrivateKey";
  private static final String COMMS_ROUTER_URL = "comms.routerUrl";
  private static final String COMMS_ROUTER_ID = "comms.routerId";
  private static final String COMMS_QUEUE_ID = "comms.queueId";
  private static final String COMMS_PLAN_ID = "comms.planId";

  private final Properties properties;

  /**
   * Loads the configuration from properties file
   *
   * @throws IOException when reading the properties file
   */
  public PropertiesConfiguration() throws IOException {

    String configPath = System.getProperty(SYSTEM_PROPERTY_KEY);
    URL propertiesFile = getFile(configPath, APPLICATION_PROPERTIES);

    LOGGER.debug("Loading file {}", propertiesFile);

    properties = new Properties();
    properties.load(propertiesFile.openStream());

    LOGGER.debug("Loaded {}", properties);
  }

  @Override
  public String callbackBaseUrl() {
    return properties.getProperty(CALLBACK_BASE_URL);
  }

  @Override
  public String nexmoCallbackBaseUrl() {
    return properties.getProperty(NEXMO_CALLBACK_BASE_URL);
  }

  @Override
  public String phone() {
    return properties.getProperty(APP_PHONE);
  }

  @Override
  public String commsRouterUrl() {
    return properties.getProperty(COMMS_ROUTER_URL);
  }

  @Override
  public String commsRouterId() {
    return properties.getProperty(COMMS_ROUTER_ID);
  }

  @Override
  public String appId() {
    return properties.getProperty(NEXMO_APP_ID);
  }

  @Override
  public String appPrivateKey() {
    return properties.getProperty(NEXMO_APP_PRIVATE_KEY);
  }

  @Override
  public String musicOnHoldUrl() {
    return properties.getProperty(APP_MUSIC_ON_HOLD_URL);
  }

  @Override
  public String commsQueueId() {
    return properties.getProperty(COMMS_QUEUE_ID);
  }

  @Override
  public String commsPlanId() {
    return properties.getProperty(COMMS_PLAN_ID);
  }

  private URL getFile(String path, String filename) throws MalformedURLException {

    File file = new File(path, filename);

    if (file.exists() && file.isFile()) {
      return file.toURI().toURL();
    }

    return Resources.getResource(filename);
  }

}
