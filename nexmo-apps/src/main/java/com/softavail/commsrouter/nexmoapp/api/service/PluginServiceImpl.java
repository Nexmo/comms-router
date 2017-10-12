package com.softavail.commsrouter.nexmoapp.api.service;

import com.google.common.collect.ImmutableMap;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.nexmoapp.interfaces.PluginService;
import com.softavail.commsrouter.nexmoapp.plugin.Plugin;
import com.softavail.commsrouter.nexmoapp.plugin.PluginProvider;
import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.stream.Collector;
import java.util.stream.StreamSupport;
import org.atteo.classindex.ClassIndex;

/**
 * Created by @author mapuo on 12.10.17.
 */
public class PluginServiceImpl implements PluginService {

  static {
    Iterable<Class<? extends Plugin>> annotated = ClassIndex.getSubclasses(Plugin.class);
    registry = StreamSupport.stream(annotated.spliterator(), true)
        .map(pluginImpl -> {
          // String name = annotation.value();
          String name = pluginImpl.getName();
          PluginProvider annotation = pluginImpl.getAnnotation(PluginProvider.class);
          if (annotation != null && !name.trim().isEmpty()) {
            name = annotation.value();
          }
          return new SimpleImmutableEntry<String, Class<? extends Plugin>>(name, pluginImpl);
        })
        .collect(Collector.of(
            ImmutableMap.Builder<String, Class<? extends Plugin>>::new,
            (builder, entry) -> builder.put(entry.getKey(), entry.getValue()),
            (builder, builder2) -> builder.putAll(builder2.build()),
            ImmutableMap.Builder::build));
  }

  @SuppressWarnings("unchecked")
  private static ImmutableMap<String, Class<? extends Plugin>> registry;

  @Override
  public Plugin findByName(String name)
      throws CommsRouterException {

    try {

      return registry.get(name).newInstance();

    } catch (NullPointerException e) {

      throw new CommsRouterException("No such plugin: " + name);
    } catch (InstantiationException | IllegalAccessException e) {

      throw new CommsRouterException("Error initializing plugin: " + name);
    }
  }

}
