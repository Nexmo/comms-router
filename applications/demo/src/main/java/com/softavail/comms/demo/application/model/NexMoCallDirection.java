package com.softavail.comms.demo.application.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum NexMoCallDirection {
  OUTBOUND, 
  INBOUND;

  @JsonValue
  @Override
  public String toString() {
    return name().toLowerCase();
  }

  @JsonCreator
  public static NexMoCallDirection fromString(String name) {
    return NexMoCallDirection.valueOf(name.toUpperCase());
  }
}
