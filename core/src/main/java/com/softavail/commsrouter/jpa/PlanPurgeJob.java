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

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.app.CoreConfiguration;
import com.softavail.commsrouter.util.ThreadPoolKiller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Date;
import java.util.List;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 *
 * @author ikrustev
 */
public class PlanPurgeJob {

  private static final Logger LOGGER = LogManager.getLogger(PlanPurgeJob.class);

  private final PlanRepository planRepo;
  private final int maxRowsPerLoop = 100;
  private final ScheduledThreadPoolExecutor threadPool;
  private final int secondsBetweenRuns;
  private final long purgeAgeSeconds;

  public PlanPurgeJob(PlanRepository planRepo, CoreConfiguration config) {
    this.planRepo = planRepo;
    this.threadPool = new ScheduledThreadPoolExecutor(1);
    this.secondsBetweenRuns = config.getPurgeJobSecondsBetweenRuns();
    this.purgeAgeSeconds = config.getPurgeJobPurgeAgeSeconds();
    this.threadPool.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
    scheduleNextRun();
  }

  public void close() {
    ThreadPoolKiller.shutdown(threadPool, "PlanPurgeJob");
  }

  private void scheduleNextRun() {
    long secondsToNextRun = secondsBetweenRuns + Math.round(secondsBetweenRuns * Math.random());
    LOGGER.info("Next run in {} seconds", secondsToNextRun);
    threadPool.schedule(this::run, secondsToNextRun, TimeUnit.SECONDS);
  }

  private void run() {
    try {
      LOGGER.info("Running...");
      purgePlans(new Date(System.currentTimeMillis() - purgeAgeSeconds * 1000));
    } catch (Exception ex) {
      LOGGER.error("Run failure: {}", ex, ex);
    }
    LOGGER.info("Run complete");
    scheduleNextRun();
  }

  private void purgePlans(Date before) throws CommsRouterException {
    List<Long> deletedPlanIds;
    int totalCount = 0;
    int failedCount = 0;
    for (;;) {
      deletedPlanIds = planRepo.getDeleted(before, maxRowsPerLoop);
      if (deletedPlanIds.isEmpty()) {
        LOGGER.info("Selected for purge: {}, failed: {}", totalCount, failedCount);
        break;
      }
      for (Long id : deletedPlanIds) {
        ++totalCount;
        try {
          planRepo.purge(id);
        } catch (CommsRouterException ex) {
          LOGGER.error("Failure purging plan ID {}: {}", id, ex, ex);
          ++failedCount;
        }
      }
    }
  }

}
