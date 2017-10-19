package com.softavail.commsrouter.webservice.config;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.cfg4j.provider.ConfigurationProvider;
import org.cfg4j.provider.ConfigurationProviderBuilder;
import org.cfg4j.source.ConfigurationSource;
import org.cfg4j.source.classpath.ClasspathConfigurationSource;
import org.cfg4j.source.context.filesprovider.ConfigFilesProvider;
import org.cfg4j.source.files.FilesConfigurationSource;
import org.cfg4j.source.inmemory.InMemoryConfigurationSource;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import javax.servlet.ServletContext;

/**
 * Created by @author mapuo on 16.10.17.
 */
public class ConfigurationImpl implements Configuration {

  private static final Logger LOGGER = LogManager.getLogger(ConfigurationImpl.class);

  private static final Properties defaultProperties;

  static {
    defaultProperties = new Properties();
    defaultProperties.setProperty("client.timeout.connect", "1500");
    defaultProperties.setProperty("client.timeout.read", "1500");
    defaultProperties.setProperty("task_dispatcher.thread_pool.size", "10");
  }

  private final ConfigurationProvider provider;

  public ConfigurationImpl(ServletContext servletContext) {
    ConfigurationSource configurationSource = getConfigFileParam(servletContext)
        .map(this::getConfigurationSource)
        .orElse(new InMemoryConfigurationSource(defaultProperties));
    provider = getProvider(configurationSource);
  }

  private Optional<Path> getConfigFileParam(ServletContext servletContext) {
    Set<String> configFileParams = Sets.newHashSet(
        servletContext.getInitParameter(Configuration.SYSTEM_PROPERTY_KEY),
        System.getProperty(Configuration.SYSTEM_PROPERTY_KEY));

    return configFileParams.stream()
        .filter(Objects::nonNull)
        .map(Paths::get)
        .filter(p -> p.toFile().exists())
        .findFirst();
  }

  private ConfigurationSource getConfigurationSource(Path configFilePath) {

    ConfigFilesProvider configFilesProvider = () ->
        ImmutableList.of(configFilePath);

    if (configFilePath.isAbsolute()) {
      LOGGER.debug("loading config from: {}", configFilePath);
      return new FilesConfigurationSource(configFilesProvider);
    } else {
      LOGGER.debug("loading config from classpath");
      return new ClasspathConfigurationSource(configFilesProvider);
    }
  }

  private ConfigurationProvider getProvider(ConfigurationSource configurationSource) {
    return new ConfigurationProviderBuilder()
        .withConfigurationSource(configurationSource)
        .build();
  }

  @Override
  public Integer getClientConnectTimeout() {
    return provider.getProperty("client.timeout.connect", Integer.class);
  }

  @Override
  public Integer getClientReadTimeout() {
    return provider.getProperty("client.timeout.read", Integer.class);
  }

  @Override
  public Integer getTaskDispatcherThreadPoolSize() {
    return provider.getProperty("task_dispatcher.thread_pool.size", Integer.class);
  }

}
