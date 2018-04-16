/*
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
  private static final String QUEUE_PROCESSOR_EVICTION_DELAY = "queue.remove.idleDelaySeconds";
  private static final String JPA_OPTIMISTIC_LOCK_RETRY_COUNT = "jpa.optimisticLock.retryCount";
  private static final String API_ENABLE_EXPRESSION_SKILL_VALIDATION =
      "api.enableExpressionSkillValidation";
  private static final String API_ENABLE_ENABLE_AGENT_CAPABILITIES_VALIDATION =
      "api.enableAgentCapabilitiesValidation";
  private static final String API_ENABLE_ENABLE_TASK_REQUIREMENTS_VALIDATION =
      "api.enableTaskRequirementsValidation";

  private static final String SHIRO_CONFIG_LOCATIONS = "shiro.configLocations";

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
    defaultProperties.setProperty(API_ENABLE_EXPRESSION_SKILL_VALIDATION,
        String.valueOf(CoreConfiguration.DEFAULT.getApiEnableExpressionSkillValidation()));
    defaultProperties.setProperty(API_ENABLE_ENABLE_AGENT_CAPABILITIES_VALIDATION,
        String.valueOf(CoreConfiguration.DEFAULT.getApiEnableAgentCapabilitiesValidation()));
    defaultProperties.setProperty(API_ENABLE_ENABLE_TASK_REQUIREMENTS_VALIDATION,
        String.valueOf(CoreConfiguration.DEFAULT.getApiEnableTaskRequirementsValidation()));

    defaultProperties.setProperty(CLIENT_TIMEOUT_CONNECT,
        String.valueOf(Configuration.DEFAULT.getClientConnectTimeout()));
    defaultProperties.setProperty(CLIENT_TIMEOUT_READ,
        String.valueOf(Configuration.DEFAULT.getClientReadTimeout()));
    defaultProperties.setProperty(CLIENT_FOLLOW_REDIRECTS,
        String.valueOf(Configuration.DEFAULT.getClientFollowRedirects()));
    defaultProperties.setProperty(SHIRO_CONFIG_LOCATIONS,
        String.valueOf(Configuration.DEFAULT.getShiroConfigLocations()));
  }

  private final ConfigurationProvider provider;

  public ConfigurationImpl(ServletContext servletContext) {
    ConfigurationSource configurationSource = getConfigFileParam(servletContext)
        .map(this::getConfigurationSource).orElse(new EmptyConfigurationSource());

    ConfigurationSource master = new SkipMissingConfigurationSource(
        new InMemoryConfigurationSource(defaultProperties), configurationSource);

    provider = getProvider(master);
  }

  private Optional<Path> getConfigFileParam(ServletContext servletContext) {
    Set<String> configFileParams =
        Sets.newHashSet(servletContext.getInitParameter(Configuration.SYSTEM_PROPERTY_KEY),
            System.getProperty(Configuration.SYSTEM_PROPERTY_KEY));

    return configFileParams.stream().filter(Objects::nonNull).map(Paths::get)
        .filter(p -> p.toFile().exists()).findFirst();
  }

  private ConfigurationSource getConfigurationSource(Path configFilePath) {

    ConfigFilesProvider configFilesProvider = () -> ImmutableList.of(configFilePath);

    if (configFilePath.isAbsolute()) {
      LOGGER.debug("loading config from: {}", configFilePath);
      return new FilesConfigurationSource(configFilesProvider);
    } else {
      LOGGER.debug("loading config from classpath");
      return new ClasspathConfigurationSource(configFilesProvider);
    }
  }

  private ConfigurationProvider getProvider(ConfigurationSource configurationSource) {
    return new ConfigurationProviderBuilder().withConfigurationSource(configurationSource).build();
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
  public Boolean getApiEnableExpressionSkillValidation() {
    return provider.getProperty(API_ENABLE_EXPRESSION_SKILL_VALIDATION, Boolean.class);
  }

  @Override
  public Boolean getApiEnableAgentCapabilitiesValidation() {
    return provider.getProperty(API_ENABLE_ENABLE_AGENT_CAPABILITIES_VALIDATION, Boolean.class);
  }

  @Override
  public Boolean getApiEnableTaskRequirementsValidation() {
    return provider.getProperty(API_ENABLE_ENABLE_TASK_REQUIREMENTS_VALIDATION, Boolean.class);
  }

  @Override
  public String getShiroConfigLocations() {
    return provider.getProperty(SHIRO_CONFIG_LOCATIONS, String.class);
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

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ConfigurationImpl{");
    sb.append("provider=").append(provider.allConfigurationAsProperties());
    sb.append('}');
    return sb.toString();
  }

}
