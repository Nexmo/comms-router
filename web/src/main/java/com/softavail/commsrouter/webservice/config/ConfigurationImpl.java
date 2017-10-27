package com.softavail.commsrouter.webservice.config;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.cfg4j.provider.ConfigurationProvider;
import org.cfg4j.provider.ConfigurationProviderBuilder;
import org.cfg4j.source.ConfigurationSource;
import org.cfg4j.source.classpath.ClasspathConfigurationSource;
import org.cfg4j.source.compose.MergeConfigurationSource;
import org.cfg4j.source.context.filesprovider.ConfigFilesProvider;
import org.cfg4j.source.empty.EmptyConfigurationSource;
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

  private static final String CLIENT_TIMEOUT_CONNECT = "client.timeout.connect";
  private static final String CLIENT_TIMEOUT_READ = "client.timeout.read";
  private static final String CLIENT_FOLLOW_REDIRECTS = "client.followRedirects";
  private static final String RETRY_DELAY_SECONDS = "client.retry.delaySeconds";
  private static final String RETRY_DELAY_MAX_SECONDS = "client.retry.delayMaxSeconds";
  private static final String RETRY_JITTER_MILLIS = "client.retry.jitterMilliseconds";
  private static final String THREAD_POOL_SIZE = "task_dispatcher.thread_pool.size";

  private static final Properties defaultProperties;

  static {
    defaultProperties = new Properties();
    defaultProperties.setProperty(CLIENT_TIMEOUT_CONNECT, "1500");
    defaultProperties.setProperty(CLIENT_TIMEOUT_READ, "1500");
    defaultProperties.setProperty(CLIENT_FOLLOW_REDIRECTS, "true");
    defaultProperties.setProperty(RETRY_DELAY_SECONDS, "2");
    defaultProperties.setProperty(RETRY_DELAY_MAX_SECONDS, "60");
    defaultProperties.setProperty(RETRY_JITTER_MILLIS, "500");
    defaultProperties.setProperty(THREAD_POOL_SIZE, "10");
  }

  private final ConfigurationProvider provider;

  public ConfigurationImpl(ServletContext servletContext) {
    ConfigurationSource configurationSource = getConfigFileParam(servletContext)
        .map(this::getConfigurationSource)
        .orElse(new EmptyConfigurationSource());

    ConfigurationSource master = new MergeConfigurationSource(
        configurationSource,
        new InMemoryConfigurationSource(defaultProperties)
    );

    provider = getProvider(master);
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
    return provider.getProperty(CLIENT_TIMEOUT_CONNECT, Integer.class);
  }

  @Override
  public Integer getClientReadTimeout() {
    return provider.getProperty(CLIENT_TIMEOUT_READ, Integer.class);
  }

  @Override
  public Boolean getClientFollowRedirects() {
    return provider.getProperty(CLIENT_FOLLOW_REDIRECTS, Boolean.class);
  }

  @Override
  public Integer getClientRetryDelaySeconds() {
    return provider.getProperty(RETRY_DELAY_SECONDS, Integer.class);
  }

  @Override
  public Integer getClientRetryDelayMaxSeconds() {
    return provider.getProperty(RETRY_DELAY_MAX_SECONDS, Integer.class);
  }

  @Override
  public Integer getClientRetryJitterMilliseconds() {
    return provider.getProperty(RETRY_JITTER_MILLIS, Integer.class);
  }

  @Override
  public Integer getTaskDispatcherThreadPoolSize() {
    return provider.getProperty(THREAD_POOL_SIZE, Integer.class);
  }

}
