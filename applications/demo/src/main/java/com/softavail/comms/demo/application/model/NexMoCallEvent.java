package com.softavail.comms.demo.application.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nexmo.client.NexmoUnexpectedException;

import java.io.IOException;

@JsonIgnoreProperties(ignoreUnknown = true)
public class NexMoCallEvent {
  private String uuid;
  private String conversationUuid;
  private NexMoCallStatus status;
  private NexMoCallDirection direction;

  /**
   * Deserialize from json. 
   * @param json json payload
   * @return NexMoCallEvent object deserialized
   */
  public static NexMoCallEvent fromJson(String json) {
    try {
      ObjectMapper mapper = new ObjectMapper();
      return mapper.readValue(json, NexMoCallEvent.class);
    } catch (IOException jpe) {
      throw new NexmoUnexpectedException("Failed to produce json from Call object.", jpe);
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

  public NexMoCallStatus getStatus() {
    return status;
  }

  public void setStatus(NexMoCallStatus status) {
    this.status = status;
  }

  public NexMoCallDirection getDirection() {
    return direction;
  }

  public void setDirection(NexMoCallDirection direction) {
    this.direction = direction;
  }
}
