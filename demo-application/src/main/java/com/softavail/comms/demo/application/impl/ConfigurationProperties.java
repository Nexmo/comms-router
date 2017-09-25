package com.softavail.comms.demo.application.impl;

/**
 * Created by @author mapuo on 29.08.17.
 */
public interface ConfigurationProperties {

  String SYSTEM_PROPERTY_KEY = "comms.demo.app.config.path";

  String callbackBaseUrl();

  String phone();

  String commsRouterUrl();

  String commsRouterID();

  String appId();

  String appPrivateKey();

  String musicOnHoldUrl();

}
