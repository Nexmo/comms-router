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

import org.junit.Test;
import static org.mockito.Mockito.*;

import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import static org.mockito.AdditionalMatchers.gt;
import org.mockito.ArgumentCaptor;

/**
 *
 * @author ikrustev
 */
public class PeriodicJobRunnerTest {

  public PeriodicJobRunnerTest() {
  }

  @Test
  public void test() {
    ScheduledThreadPoolExecutor threadPool = mock(ScheduledThreadPoolExecutor.class);
    Runnable job = mock(Runnable.class);
    doNothing().when(job).run();
    PeriodicJobRunner.start(threadPool, job, 42);
    ArgumentCaptor<Runnable> runnableCaptor = ArgumentCaptor.forClass(Runnable.class);
    verify(threadPool).schedule(
            runnableCaptor.capture(),
            gt(42L),
            eq(TimeUnit.SECONDS));
    runnableCaptor.getValue().run();
    doThrow(new RuntimeException()).when(job).run();
    runnableCaptor.getValue().run();
    verify(job, times(2)).run();
    verify(threadPool, times(3)).schedule(
            any(Runnable.class),
            gt(42L),
            eq(TimeUnit.SECONDS));
    verifyNoMoreInteractions(threadPool);
    verifyNoMoreInteractions(job);
  }

}
