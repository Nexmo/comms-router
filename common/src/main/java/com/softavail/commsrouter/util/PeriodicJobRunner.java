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

import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 *
 * @author ikrustev
 */
public class PeriodicJobRunner {

  private static final Logger LOGGER = LogManager.getLogger(PeriodicJobRunner.class);

  private final ScheduledThreadPoolExecutor threadPool;
  private final Runnable job;
  private final int secondsBetweenRuns;

  public static void start(ScheduledThreadPoolExecutor threadPool, Runnable job,
          int secondsBetweenRuns) {

    PeriodicJobRunner runner = new PeriodicJobRunner(threadPool, job, secondsBetweenRuns);
    runner.start();
  }

  public PeriodicJobRunner(ScheduledThreadPoolExecutor threadPool, Runnable job,
          int secondsBetweenRuns) {
    this.threadPool = threadPool;
    this.job = job;
    this.secondsBetweenRuns = secondsBetweenRuns;
  }

  public void start() {
    scheduleNextRun();
  }

  private void scheduleNextRun() {
    long secondsToNextRun = secondsBetweenRuns + Math.round(secondsBetweenRuns * Math.random());
    LOGGER.info("Next run in {} seconds", secondsToNextRun);
    threadPool.schedule(this::run, secondsToNextRun, TimeUnit.SECONDS);
  }

  private void run() {
    try {
      LOGGER.info("Running...");
      job.run();
    } catch (RuntimeException ex) {
      LOGGER.error("Run failure: {}", ex, ex);
    }
    LOGGER.info("Run complete");
    scheduleNextRun();
  }

}
