package com.softavail.commsrouter.app;

/**
 * Created by @author mapuo on 19.10.17.
 */
public class StateChangeEvent {

  private final String queueId;
  private final QueueProcessorState oldState;
  private final QueueProcessorState newState;

  public StateChangeEvent(String queueId,
      QueueProcessorState oldState, QueueProcessorState newState) {

    this.queueId = queueId;
    this.oldState = oldState;
    this.newState = newState;
  }

  public String getQueueId() {
    return queueId;
  }

  public QueueProcessorState getOldState() {
    return oldState;
  }

  public QueueProcessorState getNewState() {
    return newState;
  }

}
