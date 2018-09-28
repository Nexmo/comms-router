package com.softavail.comms.demo.application.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum NexMoCallStatus {
  STARTED,
  RINGING,
  ANSWERED,
  TIMEOUT,
  MACHINE,
  COMPLETED,
  FAILED,
  REJECTED,
  CANCELLED,
  BUSY;

  @JsonValue
  @Override
  public String toString() {
    return name().toLowerCase();
  }

  @JsonCreator
  public static NexMoCallStatus fromString(String name) {
    return NexMoCallStatus.valueOf(name.toUpperCase());
  }
}
