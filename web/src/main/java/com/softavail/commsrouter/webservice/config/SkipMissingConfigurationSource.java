package com.softavail.commsrouter.webservice.config;

import org.cfg4j.source.ConfigurationSource;
import org.cfg4j.source.context.environment.Environment;

import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * Created by @author mapuo on 31/10/17.
 */
public class SkipMissingConfigurationSource implements ConfigurationSource {

  private final ConfigurationSource[] sources;

  public SkipMissingConfigurationSource(ConfigurationSource... sources) {
    this.sources = Objects.requireNonNull(sources);

    for (ConfigurationSource source : sources) {
      Objects.requireNonNull(source);
    }
  }

  @Override
  public Properties getConfiguration(Environment environment) {
    Properties properties = new Properties();

    for (ConfigurationSource source : sources) {
      Properties configuration = source.getConfiguration(environment);
      Map<Object, Object> filtered = configuration.entrySet().stream()
          .filter(entry -> !entry.getValue().toString().isEmpty())
          .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
      properties.putAll(filtered);
    }

    return properties;
  }

  @Override
  public void init() {
    Arrays.stream(sources)
        .forEach(ConfigurationSource::init);
  }

  @Override
  public void reload() {
    Arrays.stream(sources)
        .forEach(ConfigurationSource::reload);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("SkipMissingConfigurationSource{");
    sb.append("sources=").append(Arrays.toString(sources));
    sb.append('}');
    return sb.toString();
  }

}
