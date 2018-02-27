package com.softavail.comms.demo.application.impl;

import com.google.common.collect.Lists;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.cfg4j.provider.ConfigurationProvider;
import org.cfg4j.provider.ConfigurationProviderBuilder;
import org.cfg4j.source.ConfigurationSource;
import org.cfg4j.source.classpath.ClasspathConfigurationSource;
import org.cfg4j.source.context.environment.ImmutableEnvironment;
import org.cfg4j.source.context.filesprovider.ConfigFilesProvider;
import org.cfg4j.source.files.FilesConfigurationSource;

import java.nio.file.Paths;

/**
 * Created by @author mapuo on 29.08.17.
 */
public class Cfg4jConfiguration implements ConfigurationProperties {

  private static final Logger LOGGER = LogManager.getLogger(Cfg4jConfiguration.class);

  private final ConfigurationProvider provider;

  public Cfg4jConfiguration() {
    ConfigurationSource source;
    ConfigFilesProvider configFilesProvider =
        () -> Lists.newArrayList(Paths.get("application.properties"));

    String configPath = System.getProperty(SYSTEM_PROPERTY_KEY);

    if (configPath != null) {
      LOGGER.debug("loading config from: {}", configPath);

      source = new FilesConfigurationSource(configFilesProvider);
      ImmutableEnvironment environment = new ImmutableEnvironment(configPath);
      provider = new ConfigurationProviderBuilder().withConfigurationSource(source)
          .withEnvironment(environment).build();

    } else {
      LOGGER.debug("loading config from classpath");

      source = new ClasspathConfigurationSource(configFilesProvider);
      provider = new ConfigurationProviderBuilder().withConfigurationSource(source).build();
    }
  }

  @Override
  public String callbackBaseUrl() {
    return provider.getProperty("app.callbackBaseUrl", String.class);
  }

  @Override
  public String nexmoCallbackBaseUrl() {
    return provider.getProperty("app.nexmoCallbackBaseUrl", String.class);
  }

  @Override
  public String phone() {
    return provider.getProperty("app.phone", String.class);
  }

  @Override
  public String commsRouterUrl() {
    return provider.getProperty("comms.routerUrl", String.class);
  }

  @Override
  public String commsRouterId() {
    return provider.getProperty("comms.routerId", String.class);
  }

  @Override
  public String appId() {
    return provider.getProperty("nexmo.appId", String.class);
  }

  @Override
  public String appPrivateKey() {
    return provider.getProperty("nexmo.appPrivateKey", String.class);
  }

  @Override
  public String musicOnHoldUrl() {
    return provider.getProperty("app.musicOnHoldUrl", String.class);
  }

  @Override
  public String commsQueueId() {
    return provider.getProperty("comms.queueId", String.class);
  }

  @Override
  public String commsPlanId() {
    return provider.getProperty("comms.planId", String.class);
  }
}
