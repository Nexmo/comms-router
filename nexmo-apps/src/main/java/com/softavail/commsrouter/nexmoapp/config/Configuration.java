package com.softavail.commsrouter.nexmoapp.config;

/**
 * Created by @author mapuo on 11.10.17.
 */
public interface Configuration {

  String SYSTEM_PROPERTY_KEY = "nexmo.apps.config.file";

  String getCommsRouterUrl();

  String getCallbackBaseUrl();

}
