package com.softavail.commsrouter.app;

/**
 * Created by @author mapuo on 18.10.17.
 */
@FunctionalInterface
public interface StateWaitingListener extends StateChangeListener {

  default void stateChanged(
      String queueId, QueueProcessorState oldState, QueueProcessorState newState) {

    if (newState == QueueProcessorState.WAIT) {
      inWaitingState(queueId);
    }

  }

  void inWaitingState(String queueId);

}
