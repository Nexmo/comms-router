package com.softavail.comms.demo.application.services;

import com.nexmo.client.auth.JWTAuthMethod;
import com.nexmo.client.voice.Endpoint;

/**
 * Created by @author mapuo on 29.08.17.
 */
public interface Configuration {

  JWTAuthMethod getJwtAuthMethod();

  Endpoint getAssociatedPhone();

  String getCallbackBaseUrl();

  String getNexmoCallbackBaseUrl();

  String getCommsApiEndpoint();

  String getCommsRouterId();

  String getMusicOnHoldUrl();

  String getCommsQueueId();

  String getCommsPlanId();
}
