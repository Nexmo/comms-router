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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Date;
import java.util.List;

/**
 *
 * @author ikrustev
 */
public class PlanPurgeJob implements Runnable {

  private static final Logger LOGGER = LogManager.getLogger(PlanPurgeJob.class);
  public static final int MAX_ROWS_PER_LOOP = 100;

  private final PlanRepository planRepo;
  private final long purgeAgeSeconds;

  public PlanPurgeJob(PlanRepository planRepo, long purgeAgeSeconds) {
    this.planRepo = planRepo;
    this.purgeAgeSeconds = purgeAgeSeconds;
  }

  @Override
  public void run() {
    try {
      purgePlans(new Date(System.currentTimeMillis() - purgeAgeSeconds * 1000));
    } catch (CommsRouterException ex) {
      throw new RuntimeException(ex);
    }
  }

  private void purgePlans(Date before) throws CommsRouterException {
    List<Long> deletedPlanIds;
    int totalCount = 0;
    int failedCount = 0;
    for (;;) {
      deletedPlanIds = planRepo.getDeleted(before, MAX_ROWS_PER_LOOP);
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
