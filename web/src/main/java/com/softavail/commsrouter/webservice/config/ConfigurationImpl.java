package com.softavail.commsrouter.webservice.config;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;

import com.softavail.commsrouter.app.CoreConfiguration;
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
public class ConfigurationImpl implements CoreConfiguration, Configuration {

  private static final Logger LOGGER = LogManager.getLogger(ConfigurationImpl.class);

  private static final String CLIENT_TIMEOUT_CONNECT = "client.timeout.connect";
  private static final String CLIENT_TIMEOUT_READ = "client.timeout.read";
  private static final String CLIENT_FOLLOW_REDIRECTS = "client.followRedirects";
  private static final String BACKOFF_DELAY_SECONDS = "client.retry.delaySeconds";
  private static final String BACKOFF_DELAY_MAX_SECONDS = "client.retry.delayMaxSeconds";
  private static final String BACKOFF_JITTER_MILLIS = "client.retry.jitterMilliseconds";
  private static final String THREAD_POOL_SIZE = "task_dispatcher.thread_pool.size";
  private static final String THREAD_POOL_SHUTDOWN_TIMEOUT =
      "task_dispatcher.thread_pool.shutdown.delaySeconds";
  private static final String QUEUE_RETRY_DELAY_SECONDS = "queue.retry.delaySeconds";
  private static final String QUEUE_PROCESSOR_EVICTION_DELAY =
      "queue.remove.idleDelaySeconds";
  private static final String JPA_OPTIMISTIC_LOCK_RETRY_COUNT =
      "jpa.optimisticLock.retryCount";

  private static final Properties defaultProperties;

  static {
    defaultProperties = new Properties();

    defaultProperties.setProperty(BACKOFF_DELAY_SECONDS,
        String.valueOf(CoreConfiguration.DEFAULT.getBackoffDelay()));
    defaultProperties.setProperty(BACKOFF_DELAY_MAX_SECONDS,
        String.valueOf(CoreConfiguration.DEFAULT.getBackoffDelayMax()));
    defaultProperties.setProperty(BACKOFF_JITTER_MILLIS,
        String.valueOf(CoreConfiguration.DEFAULT.getJitter()));
    defaultProperties.setProperty(THREAD_POOL_SIZE,
        String.valueOf(CoreConfiguration.DEFAULT.getDispatcherThreadPoolSize()));
    defaultProperties.setProperty(THREAD_POOL_SHUTDOWN_TIMEOUT,
        String.valueOf(CoreConfiguration.DEFAULT.getDispatcherThreadShutdownDelay()));
    defaultProperties.setProperty(QUEUE_PROCESSOR_EVICTION_DELAY,
        String.valueOf(CoreConfiguration.DEFAULT.getQueueProcessRetryDelay()));
    defaultProperties.setProperty(QUEUE_RETRY_DELAY_SECONDS,
        String.valueOf(CoreConfiguration.DEFAULT.getQueueProcessRetryDelay()));
    defaultProperties.setProperty(JPA_OPTIMISTIC_LOCK_RETRY_COUNT,
        String.valueOf(CoreConfiguration.DEFAULT.getJpaLockRetryCount()));

    defaultProperties.setProperty(CLIENT_TIMEOUT_CONNECT,
        String.valueOf(Configuration.DEFAULT.getClientConnectTimeout()));
    defaultProperties.setProperty(CLIENT_TIMEOUT_READ,
        String.valueOf(Configuration.DEFAULT.getClientReadTimeout()));
    defaultProperties.setProperty(CLIENT_FOLLOW_REDIRECTS,
        String.valueOf(Configuration.DEFAULT.getClientFollowRedirects()));
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
  public Integer getBackoffDelay() {
    return provider.getProperty(BACKOFF_DELAY_SECONDS, Integer.class);
  }

  @Override
  public Integer getBackoffDelayMax() {
    return provider.getProperty(BACKOFF_DELAY_MAX_SECONDS, Integer.class);
  }

  @Override
  public Integer getJitter() {
    return provider.getProperty(BACKOFF_JITTER_MILLIS, Integer.class);
  }

  @Override
  public Integer getDispatcherThreadPoolSize() {
    return provider.getProperty(THREAD_POOL_SIZE, Integer.class);
  }

  @Override
  public Integer getDispatcherThreadShutdownDelay() {
    return provider.getProperty(THREAD_POOL_SHUTDOWN_TIMEOUT, Integer.class);
  }

  @Override
  public Integer getQueueProcessRetryDelay() {
    return provider.getProperty(QUEUE_RETRY_DELAY_SECONDS, Integer.class);
  }

  @Override
  public Long getQueueProcessorEvictionDelay() {
    return provider.getProperty(QUEUE_PROCESSOR_EVICTION_DELAY, Long.class);
  }

  @Override
  public Integer getJpaLockRetryCount() {
    return provider.getProperty(JPA_OPTIMISTIC_LOCK_RETRY_COUNT, Integer.class);
  }

}
