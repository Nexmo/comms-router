package com.softavail.commsrouter.app;

/**
 * Created by @author mapuo on 27.10.17.
 */
public interface CoreConfiguration {

  CoreConfiguration DEFAULT = new CoreConfiguration() {
    // Nothing to implement, all methods should have default modifier
  };

  default Integer getBackoffDelay() {
    return 2;
  }

  default Integer getBackoffDelayMax() {
    return 60;
  }

  default Integer getJitter() {
    return 500;
  }

  default Integer getDispatcherThreadPoolSize() {
    return 10;
  }

  default Integer getDispatcherThreadShutdownDelay() {
    return 10;
  }

  default Integer getQueueProcessRetryDelay() {
    return 10;
  }

  default Long getQueueProcessorEvictionDelay() {
    return 10L;
  }

  default Integer getJpaLockRetryCount() {
    return 10;
  }

}
