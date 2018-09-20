package com.softavail.comms.demo.application.model;

public enum NexMoConversationStatus {
  STARTED,
  WAITING,
  CONNECTING,
  CONNECTED,
  TIMEOUT,
  COMPLETED;

  @Override
  public String toString() {
    return name().toLowerCase();
  }

}
