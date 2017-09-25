package com.softavail.comms.demo.application.model;

import com.nexmo.client.voice.CallDirection;
import com.nexmo.client.voice.CallStatus;

public class NexMoCall {
  private String uuid;
  private String conversationUuid;
  private CallStatus status;
  private CallDirection direction;

  /** .
   * Constructor
   */
  public NexMoCall(String uuid, String conversationUuid) {
    this.uuid = uuid;
    this.conversationUuid = conversationUuid;
  }

  public String getUuid() {
    return uuid;
  }

  public String getConversationUuid() {
    return conversationUuid;
  }

  public CallStatus getStatus() {
    return status;
  }

  public void setStatus(CallStatus status) {
    this.status = status;
  }

  public CallDirection getDirection() {
    return direction;
  }

  public void setDirection(CallDirection direction) {
    this.direction = direction;
  }
  
  /**
   * .
   */
  @Override
  public NexMoCall clone() {
    NexMoCall call = new NexMoCall(this.getUuid(), this.getConversationUuid());

    if (this.getDirection() != null) {
      call.setDirection(this.getDirection());
    }
    if (this.getStatus() != null) {
      call.setStatus(this.getStatus());
    }

    return call;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder()
        .append("NexMoCall [")
        .append("uuid = ").append(uuid)
        .append(", convId = ").append(conversationUuid)
        .append(", status = ").append(status.toString())
        .append(", direction = ").append(direction.toString())
        .append("]");
    return sb.toString();
  }
}
