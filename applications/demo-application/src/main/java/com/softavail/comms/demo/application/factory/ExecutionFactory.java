package com.softavail.comms.demo.application.factory;

import org.glassfish.hk2.api.Factory;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;

/**
 * Created by @author mapuo on 29.08.17.
 */
public class ExecutionFactory implements Factory<ScheduledExecutorService> {

  private static ScheduledThreadPoolExecutor executor;

  public ExecutionFactory() {
    executor = new ScheduledThreadPoolExecutor(1);
  }

  @Override
  public ScheduledExecutorService provide() {
    return executor;
  }

  @Override
  public void dispose(ScheduledExecutorService instance) {
    executor.shutdown();
  }

}
