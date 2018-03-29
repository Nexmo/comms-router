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

import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

/**
 *
 * @author ikrustev
 */
public class ThreadPoolKillerTest {

  public ThreadPoolKillerTest() {
  }

  @Test
  public void testCleanShutdown() throws InterruptedException {
    final int wait = 1;
    ExecutorService threadPool = mock(ExecutorService.class);
    when(threadPool.awaitTermination(wait, TimeUnit.SECONDS)).thenReturn(true);
    ThreadPoolKiller.shutdown(threadPool, "TestPool", wait);
    verify(threadPool).shutdown();
    verify(threadPool).awaitTermination(wait, TimeUnit.SECONDS);
  }

  @Test
  public void testForcedShutdown() throws InterruptedException {
    final int wait = 1;
    ExecutorService threadPool = mock(ExecutorService.class);
    when(threadPool.awaitTermination(wait, TimeUnit.SECONDS)).thenReturn(false).thenReturn(true);
    ThreadPoolKiller.shutdown(threadPool, "TestPool", wait);
    verify(threadPool).shutdown();
    verify(threadPool, times(2)).awaitTermination(wait, TimeUnit.SECONDS);
    verify(threadPool).shutdownNow();
    verifyNoMoreInteractions(threadPool);
  }

  @Test
  public void testForcedShutdownTimesOut() throws InterruptedException {
    final int wait = 1;
    ExecutorService threadPool = mock(ExecutorService.class);
    when(threadPool.awaitTermination(wait, TimeUnit.SECONDS)).thenReturn(false).thenReturn(false);
    ThreadPoolKiller.shutdown(threadPool, "TestPool", wait);
    verify(threadPool).shutdown();
    verify(threadPool, times(2)).awaitTermination(wait, TimeUnit.SECONDS);
    verify(threadPool).shutdownNow();
    verifyNoMoreInteractions(threadPool);
  }

  @Test
  public void testShutdownInterrupted() throws InterruptedException {
    final int wait = 1;
    ExecutorService threadPool = mock(ExecutorService.class);
    when(threadPool.awaitTermination(wait, TimeUnit.SECONDS)).thenThrow(InterruptedException.class);
    ThreadPoolKiller.shutdown(threadPool, "TestPool", wait);
    verify(threadPool).shutdown();
    verify(threadPool).awaitTermination(wait, TimeUnit.SECONDS);
    verifyNoMoreInteractions(threadPool);
  }

  @Test
  public void testShutdownDefaultTimeout() throws InterruptedException {
    final long wait = ThreadPoolKiller.DEFAULT_WAIT_DOWN_SECONDS;
    ExecutorService threadPool = mock(ExecutorService.class);
    when(threadPool.awaitTermination(wait, TimeUnit.SECONDS)).thenReturn(true);
    ThreadPoolKiller.shutdown(threadPool, "TestPool");
    verify(threadPool).shutdown();
    verify(threadPool).awaitTermination(wait, TimeUnit.SECONDS);
    verifyNoMoreInteractions(threadPool);
  }

  @Test
  public void testInstantiate() {
    new ThreadPoolKiller();
  }

}
