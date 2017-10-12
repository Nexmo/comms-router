package com.softavail.commsrouter.nexmoapp.config;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.cfg4j.provider.ConfigurationProvider;
import org.cfg4j.provider.ConfigurationProviderBuilder;
import org.cfg4j.source.ConfigurationSource;
import org.cfg4j.source.classpath.ClasspathConfigurationSource;
import org.cfg4j.source.context.environment.ImmutableEnvironment;
import org.cfg4j.source.context.filesprovider.ConfigFilesProvider;
import org.cfg4j.source.files.FilesConfigurationSource;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

/**
 * Created by @author mapuo on 11.10.17.
 */
@WebListener("Provide Configuration")
public class ConfigurationImpl implements ServletContextListener, Configuration {

  private static final Logger LOGGER = LogManager.getLogger(ConfigurationImpl.class);

  private static ConfigurationProvider provider;

  @Override
  public String getCommsRouterUrl() {
    return provider.getProperty("comms.routerUrl", String.class);
  }

  @Override
  public String getCallbackBaseUrl() {
    return provider.getProperty("app.callbackBaseUrl", String.class);
  }

  @Override
  public void contextInitialized(ServletContextEvent sce) {

    final Path configFilePath = getConfigFileParam(sce.getServletContext());
    ConfigFilesProvider configFilesProvider = () ->
        Lists.newArrayList(configFilePath);

    if (configFilePath.isAbsolute()) {
      LOGGER.debug("loading config from: {}", configFilePath);

      ConfigurationSource source = new FilesConfigurationSource(configFilesProvider);
      ImmutableEnvironment environment = new ImmutableEnvironment(configFilePath.toString());
      provider = new ConfigurationProviderBuilder()
          .withConfigurationSource(source)
          .withEnvironment(environment)
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
  public void contextDestroyed(ServletContextEvent sce) {
    // Nothing to do
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
