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

import org.junit.Test;
import static org.mockito.Mockito.*;

import java.util.Arrays;
import java.util.Collections;

/**
 *
 * @author ikrustev
 */
public class PlanPurgeJobTest {

  public PlanPurgeJobTest() {
  }

  @Test
  public void test() throws CommsRouterException {
    int ageSeconds = 10;
    final long beforeStamp = System.currentTimeMillis() - ageSeconds * 1000;
    PlanRepository planRepo = mock(PlanRepository.class);
    when(planRepo.getDeleted(any(), anyInt()))
            .thenReturn(Arrays.asList(10L, 20L))
            .thenReturn(Arrays.asList(1L, 2L))
            .thenReturn(Collections.emptyList());
    doThrow(new CommsRouterException()).when(planRepo).purge(1L);
    PlanPurgeJob purgeJob = new PlanPurgeJob(planRepo, ageSeconds);
    purgeJob.run();
    verify(planRepo, times(3)).getDeleted(
            argThat(arg -> Math.abs(arg.getTime() - beforeStamp) < 10000),
            eq(PlanPurgeJob.MAX_ROWS_PER_LOOP));
    verify(planRepo).purge(10L);
    verify(planRepo).purge(20L);
    verify(planRepo).purge(1L);
    verify(planRepo).purge(2L);
    verifyNoMoreInteractions(planRepo);
  }

  @Test(expected = RuntimeException.class)
  public void testgetDeletedThrows() throws CommsRouterException {
    int ageSeconds = 10;
    PlanRepository planRepo = mock(PlanRepository.class);
    when(planRepo.getDeleted(any(), anyInt())).thenThrow(CommsRouterException.class);
    PlanPurgeJob purgeJob = new PlanPurgeJob(planRepo, ageSeconds);
    purgeJob.run();
    verify(planRepo, times(1)).getDeleted(any(), anyInt());
    verifyNoMoreInteractions(planRepo);
  }

}
