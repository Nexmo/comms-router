package com.softavail.comms.nexmo.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum NexmoCallStatus {
  STARTED, 
  RINGING, 
  ANSWERED, 
  MACHINE, 
  COMPLETED,
  TIMEOUT,
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
  public static NexmoCallStatus fromString(String name) {
    return NexmoCallStatus.valueOf(name.toUpperCase());
  }

}
