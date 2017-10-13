package com.softavail.comms.nexmo.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum NexmoCallDirection {
  OUTBOUND, 
  INBOUND;

  @JsonValue
  @Override
  public String toString() {
    return name().toLowerCase();
  }

  @JsonCreator
  public static NexmoCallDirection fromString(String name) {
    return NexmoCallDirection.valueOf(name.toUpperCase());
  }
}
