package com.softavail.commsrouter.nexmoapp.config;

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

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import javax.servlet.ServletContext;

/**
 * Created by @author mapuo on 11.10.17.
 */
public class ConfigurationImpl implements Configuration {

  private static final Logger LOGGER = LogManager.getLogger(ConfigurationImpl.class);

  private final ConfigurationProvider provider;

  public ConfigurationImpl(ServletContext servletContext) {
    final Path configFilePath = getConfigFileParam(servletContext);
    ConfigFilesProvider configFilesProvider = () ->
        ImmutableList.of(configFilePath);

    if (configFilePath.isAbsolute()) {
      LOGGER.debug("loading config from: {}", configFilePath);

      ConfigurationSource source = new FilesConfigurationSource(configFilesProvider);
      provider = new ConfigurationProviderBuilder()
          .withConfigurationSource(source)
          .build();

    } else {
      LOGGER.debug("loading config from classpath");

      ConfigurationSource source = new ClasspathConfigurationSource(configFilesProvider);
      provider = new ConfigurationProviderBuilder()
          .withConfigurationSource(source)
          .build();
    }
  }

  @Override
  public String getCommsRouterUrl() {
    return provider.getProperty("app.commsRouterUrl", String.class);
  }

  @Override
  public String getCallbackBaseUrl() {
    return provider.getProperty("app.callbackBaseUrl", String.class);
  }

  @Override
  public int getThreadPoolSize() {
    return provider.getProperty("app.threadPoolSize", Integer.class);
  }

  private Path getConfigFileParam(ServletContext servletContext) {
    Set<String> configFileParams = Sets.newHashSet(
        servletContext.getInitParameter(Configuration.SYSTEM_PROPERTY_KEY),
        System.getProperty(Configuration.SYSTEM_PROPERTY_KEY));

    Optional<Path> path = configFileParams.stream()
        .filter(Objects::nonNull)
        .map(Paths::get)
        .filter(p -> p.toFile().exists())
        .findFirst();

    return path.orElseThrow(() -> new IllegalArgumentException(
        "Missing configuration file argument! "
            + "Please provide JVM option (-D" + Configuration.SYSTEM_PROPERTY_KEY + ") "
            + "or Servlet context-param with the configuration file."));
  }

}
