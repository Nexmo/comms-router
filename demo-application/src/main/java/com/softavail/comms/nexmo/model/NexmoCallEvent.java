package com.softavail.comms.nexmo.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;

@JsonIgnoreProperties(ignoreUnknown = true)
public class NexmoCallEvent {
  private String uuid;
  private String conversationUuid;
  private NexmoCallStatus status;
  private NexmoCallDirection direction;

  public static NexmoCallEvent fromJson(String json) {
    try {
      ObjectMapper mapper = new ObjectMapper();
      return mapper.readValue(json, NexmoCallEvent.class);
    } catch (IOException jpe) {
      throw new RuntimeException("Failed to produce json from Call object.", jpe);
    }
  }

  public String getUuid() {
    return uuid;
  }

  public void setUuid(String uuid) {
    this.uuid = uuid;
  }

  @JsonProperty("conversation_uuid")
  public String getConversationUuid() {
    return conversationUuid;
  }

  public void setConversationUuid(String conversationUuid) {
    this.conversationUuid = conversationUuid;
  }

  public NexmoCallStatus getStatus() {
    return status;
  }

  public void setStatus(NexmoCallStatus status) {
    this.status = status;
  }

  public NexmoCallDirection getDirection() {
    return direction;
  }

  public void setDirection(NexmoCallDirection direction) {
    this.direction = direction;
  }
}
