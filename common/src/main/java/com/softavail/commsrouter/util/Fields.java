/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.util;

import java.util.Map;
import java.util.function.Consumer;

/**
 * @author ikrustev
 */
public class Fields {

  public static void update(Consumer<String> consumer, String oldValue, String newValue) {
    if (newValue == null) {
      return;
    }
    if (newValue.isEmpty()) {
      consumer.accept(null);
    } else {
      consumer.accept(newValue);
    }
  }

  public static <T extends Map> void update(Consumer<T> consumer, T oldValue, T newValue) {
    if (newValue == null) {
      return;
    }
    if (newValue.isEmpty()) {
      consumer.accept(null);
    } else {
      consumer.accept(newValue);
    }
  }

  public static <T extends Enum<?>> void update(Consumer<T> consumer, T oldValue, T newValue) {
    if (newValue == null) {
      return;
    }
    consumer.accept(newValue);
  }

  public static <T> void update(Consumer<T> consumer, T oldValue, T newValue) {
    if (newValue == null) {
      return;
    }
    consumer.accept(newValue);
  }

  /**
   * Updates a database filed with NULL value
   * @param consumer Performs this operation on the given consumer.
   * @param oldValue The old value for the field
   */
  public static <T> void nullify(Consumer<T> consumer, T oldValue) {
    consumer.accept(null);
  }
}
