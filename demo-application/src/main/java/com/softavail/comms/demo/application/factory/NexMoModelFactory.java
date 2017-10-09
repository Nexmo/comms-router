package com.softavail.comms.demo.application.factory;

import com.nexmo.client.voice.Endpoint;
import com.nexmo.client.voice.PhoneEndpoint;
import com.nexmo.client.voice.SipEndpoint;

public class NexMoModelFactory {

  public static Endpoint createEndpoint(String value) {
    Endpoint endpoint = null;
    
    if (null != value) {
      if (value.startsWith("sip:")) {
        endpoint = new SipEndpoint(value);
      } else {
        endpoint = new PhoneEndpoint(value);
      }
    }
    
    return endpoint;
  }
}
