/*
 * Copyright 2018 SoftAvail Inc.
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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

/**
 *
 * @author ikrustev
 */
public class ThreadPoolKiller {

  private static final Logger LOGGER = LogManager.getLogger(ThreadPoolKiller.class);

  public static final long DEFAULT_WAIT_DOWN_SECONDS = 10;

  public static void shutdown(ExecutorService threadPool, String name) {
    shutdown(threadPool, name, DEFAULT_WAIT_DOWN_SECONDS);
  }

  public static void shutdown(ExecutorService threadPool, String name, long waitSeconds) {
    LOGGER.info("Thread pool '{}': going down", name);
    threadPool.shutdown();
    try {
      if (threadPool.awaitTermination(waitSeconds, TimeUnit.SECONDS)) {
        LOGGER.info("Thread pool '{}': down", name);
      } else {
        LOGGER.warn("Thread pool '{}': shutdown timeout. Forcing ...", name);
        threadPool.shutdownNow();
        if (threadPool.awaitTermination(waitSeconds, TimeUnit.SECONDS)) {
          LOGGER.info("Thread pool '{}': forced down", name);
        } else {
          LOGGER.error("Thread pool '{}': forced shutdown timeout", name);
        }
      }
    } catch (InterruptedException ex) {
      LOGGER.error("Thread pool '{}': interrupted while waiting to go down", name);
    }
  }
}
