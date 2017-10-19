package com.softavail.commsrouter.nexmoapp.plugin;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Created by @author mapuo on 12.10.17.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface PluginProvider {

  /**
   * Overrides the name of the plugin
   */
  String value();

}
