package com.softavail.comms.demo.application.impl;

import com.google.common.io.Resources;

import com.nexmo.client.auth.JWTAuthMethod;
import com.nexmo.client.voice.Endpoint;
import com.softavail.comms.demo.application.factory.NexMoModelFactory;
import com.softavail.comms.demo.application.services.Configuration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import javax.inject.Inject;

/**
 * Created by @author mapuo on 30.08.17.
 */
public class ConfigurationImpl implements Configuration {

  private static final Logger LOGGER = LogManager.getLogger(ConfigurationImpl.class);

  private final ConfigurationProperties properties;
  private final Endpoint phoneEndpoint;
  private final JWTAuthMethod jwtAuthMethod;
  private final String commsApiEndpoint;
  private final String commsRouterId;
  private final String callbackBaseUrl;
  private final String nexmoCallbackBaseUrl;
  private final String musicOnHoldUrl;
  private final String commsQueueId;
  private final String commsPlanId;

  @Inject
  public ConfigurationImpl(ConfigurationProperties properties) {
    this.properties = properties;

    callbackBaseUrl = addTrailingSlash(properties.callbackBaseUrl());
    LOGGER.debug("Callback Url: {}", callbackBaseUrl);

    nexmoCallbackBaseUrl = addTrailingSlash(properties.nexmoCallbackBaseUrl());
    LOGGER.debug("Nexmo callback Url: {}", nexmoCallbackBaseUrl);

    commsApiEndpoint = addTrailingSlash(properties.commsRouterUrl());
    LOGGER.debug("Comms Api Endpoint: {}", commsApiEndpoint);

    commsRouterId = properties.commsRouterId();

    phoneEndpoint = NexMoModelFactory.createEndpoint(properties.phone());
    LOGGER.debug("phoneEndpoint: {}", phoneEndpoint);

    try {
      String configPath = System.getProperty(PropertiesConfiguration.SYSTEM_PROPERTY_KEY);
      URL privateKeyFile = getFile(configPath, properties.appPrivateKey());
      byte[] privateKey = Resources.toByteArray(privateKeyFile);
      jwtAuthMethod = new JWTAuthMethod(properties.appId(), privateKey);
      LOGGER.debug("private key loaded from: {}", privateKeyFile);
    } catch (Exception e) {
      throw new RuntimeException("Can't read private key", e);
    }

    // set music on hold URL
    musicOnHoldUrl = properties.musicOnHoldUrl();

    commsQueueId = properties.commsQueueId();

    commsPlanId = properties.commsPlanId();
  }

  @Override
  public JWTAuthMethod getJwtAuthMethod() {
    return jwtAuthMethod;
  }

  @Override
  public Endpoint getAssociatedPhone() {
    return phoneEndpoint;
  }

  @Override
  public String getCallbackBaseUrl() {
    return callbackBaseUrl;
  }

  @Override
  public String getNexmoCallbackBaseUrl() {
    return nexmoCallbackBaseUrl;
  }

  @Override
  public String getCommsApiEndpoint() {
    return commsApiEndpoint;
  }

  @Override
  public String getCommsRouterId() {
    return commsRouterId;
  }

  @Override
  public String getMusicOnHoldUrl() {
    return musicOnHoldUrl;
  }

  @Override
  public String getCommsQueueId() {
    return commsQueueId;
  }

  @Override
  public String getCommsPlanId() {
    return commsPlanId;
  }

  private URL getFile(String path, String filename) throws MalformedURLException {

    File file = new File(path, filename);

    if (file.exists() && file.isFile()) {
      return file.toURI().toURL();
    }

    return Resources.getResource(filename);
  }

  private String addTrailingSlash(String path) {
    if (!path.endsWith("/")) {
      return path + "/";
    }
    return path;
  }

}
