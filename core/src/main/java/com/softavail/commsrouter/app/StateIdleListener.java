package com.softavail.commsrouter.app;

/**
 * Created by @author mapuo on 18.10.17.
 */
@FunctionalInterface
public interface StateIdleListener extends StateChangeListener {

  default void stateChanged(StateChangeEvent changeEvent) {

    if (changeEvent.getNewState() == QueueProcessorState.IDLE) {
      inWaitingState(changeEvent.getQueueId());
    }

  }

  void inWaitingState(String queueId);

}
