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

import com.google.common.collect.ImmutableMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.cfg4j.provider.ConfigurationProvider;
import org.cfg4j.provider.ConfigurationProviderBuilder;
import org.cfg4j.source.ConfigurationSource;
import org.cfg4j.source.compose.MergeConfigurationSource;
import org.cfg4j.source.empty.EmptyConfigurationSource;
import org.cfg4j.source.inmemory.InMemoryConfigurationSource;

import java.io.InputStream;
import java.util.AbstractMap.SimpleEntry;
import java.util.Map;
import java.util.Properties;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import javax.servlet.ServletContext;

/**
 * Created by @author mapuo on 30/10/17.
 */
public class ManifestConfigurationImpl implements ManifestConfiguration {

  private static final Logger LOGGER = LogManager.getLogger(ManifestConfigurationImpl.class);

  private static final String MANIFEST_MF = "/META-INF/MANIFEST.MF";

  private static final String MANIFEST_TITLE = "ImplementationTitle";
  private static final String MANIFEST_VERSION = "ImplementationVersion";
  private static final String MANIFEST_BUILD = "ImplementationBuild";
  private static final String MANIFEST_BUILD_TIME = "ImplementationBuildTime";
  private static final String MANIFEST_BUILD_JDK = "BuildJdk";
  private final ConfigurationProvider provider;

  public ManifestConfigurationImpl(ServletContext servletContext) {

    ConfigurationSource manifestConfiguration = getManifestConfiguration(servletContext);
    provider = new ConfigurationProviderBuilder()
        .withConfigurationSource(manifestConfiguration)
        .build();
  }

  private ConfigurationSource getManifestConfiguration(ServletContext servletContext) {

    Map<String, String> defaultManifestMap = new ImmutableMap.Builder<String, String>()
        .put(MANIFEST_TITLE, "Comms Router Web")
        .put(MANIFEST_VERSION, "NOT_AVAILABLE")
        .put(MANIFEST_BUILD, "NOT_AVAILABLE")
        .put(MANIFEST_BUILD_TIME, "NOT_AVAILABLE")
        .put(MANIFEST_BUILD_JDK, "NOT_AVAILABLE")
        .build();

    Properties defaultManifestProperties = new Properties();
    defaultManifestProperties.putAll(defaultManifestMap);
    InMemoryConfigurationSource defaultManifestSource =
        new InMemoryConfigurationSource(defaultManifestProperties);

    ConfigurationSource manifestSource = new EmptyConfigurationSource();

    try {

      final Properties manifestProperties = new Properties();
      try (InputStream stream = servletContext.getResourceAsStream(MANIFEST_MF)) {
        Manifest manifest = new Manifest(stream);
        Attributes mainAttributes = manifest.getMainAttributes();
        Map<String, Object> collect = mainAttributes.entrySet().stream()
            .filter(entry -> !entry.getValue().toString().isEmpty())
            .map(entry -> {
              String key = entry.getKey().toString().replace("-", "");
              return new SimpleEntry<>(key, entry.getValue());
            })
            .collect(Collectors.toMap(SimpleEntry::getKey, SimpleEntry::getValue));
        manifestProperties.putAll(collect);
      }

      manifestSource = new InMemoryConfigurationSource(manifestProperties);

    } catch (Exception e) {
      LOGGER.warn("Could not load the MANIFEST.MF file!", e);
    }

    MergeConfigurationSource mergeConfigurationSource =
        new MergeConfigurationSource(defaultManifestSource, manifestSource);

    LOGGER.debug("manifestProperties: {}", mergeConfigurationSource);

    return mergeConfigurationSource;
  }

  @Override
  public String getImplementationTitle() {
    return provider.getProperty(MANIFEST_TITLE, String.class);
  }

  @Override
  public String getImplementationVersion() {
    return provider.getProperty(MANIFEST_VERSION, String.class);
  }

  @Override
  public String getImplementationBuild() {
    return provider.getProperty(MANIFEST_BUILD, String.class);
  }

  @Override
  public String getImplementationBuildTime() {
    return provider.getProperty(MANIFEST_BUILD_TIME, String.class);
  }

  @Override
  public String getBuildJdk() {
    return provider.getProperty(MANIFEST_BUILD_JDK, String.class);
  }

}
