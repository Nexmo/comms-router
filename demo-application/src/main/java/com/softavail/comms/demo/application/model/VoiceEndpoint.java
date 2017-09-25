package com.softavail.comms.demo.application.model;

import com.nexmo.client.voice.Endpoint;
import com.nexmo.client.voice.PhoneEndpoint;
import com.nexmo.client.voice.SipEndpoint;

/**
 * Created by @author mapuo on 29.08.17.
 */
public class VoiceEndpoint implements Endpoint {

  private Endpoint endpoint;

  public VoiceEndpoint(String endpoint) {
    if (endpoint.startsWith("sip:")) {
      this.endpoint = new SipEndpoint(endpoint);
    }
    this.endpoint = new PhoneEndpoint(endpoint);
  }

  @Override
  public String getType() {
    return endpoint.getType();
  }

  @Override
  public String toLog() {
    return endpoint.toLog();
  }

  @Override
  public String toString() {
    return "VoiceEndpoint{"
        + "type=" + endpoint.getType()
        + ", log=" + endpoint.toLog()
        + '}';
  }

}

