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
}
