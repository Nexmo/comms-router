package com.softavail.commsrouter.app;

/**
 * Created by @author mapuo on 27.10.17.
 */
public interface CoreConfiguration {

  CoreConfiguration DEFAULT = new CoreConfiguration() {
    @Override
    public Integer getBackoffDelay() {
      return 2;
    }

    @Override
    public Integer getBackoffDelayMax() {
      return 60;
    }

    @Override
    public Integer getJitter() {
      return 500;
    }

    @Override
    public Integer getDispatcherThreadPoolSize() {
      return 10;
    }

    @Override
    public Integer getDispatcherThreadShutdownDelay() {
      return 10;
    }

    @Override
    public Integer getQueueProcessRetryDelay() {
      return 10;
    }

    @Override
    public Long getQueueProcessorEvictionDelay() {
      return 10L;
    }

    @Override
    public Integer getJpaLockRetryCount() {
      return 10;
    }
  };

  Integer getBackoffDelay();

  Integer getBackoffDelayMax();

  Integer getJitter();

  Integer getDispatcherThreadPoolSize();

  Integer getDispatcherThreadShutdownDelay();

  Integer getQueueProcessRetryDelay();

  Long getQueueProcessorEvictionDelay();

  Integer getJpaLockRetryCount();

}
