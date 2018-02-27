package com.softavail.comms.demo.application.impl;

/**
 * Created by @author mapuo on 29.08.17.
 */
public interface ConfigurationProperties {

  String SYSTEM_PROPERTY_KEY = "comms.demo.app.config.path";

  String callbackBaseUrl();

  String nexmoCallbackBaseUrl();

  String phone();

  String commsRouterUrl();

  String commsRouterId();

  String appId();

  String appPrivateKey();

  String musicOnHoldUrl();

  String commsQueueId();

  String commsPlanId();
}
