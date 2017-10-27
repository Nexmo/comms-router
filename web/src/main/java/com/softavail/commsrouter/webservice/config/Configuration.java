package com.softavail.commsrouter.webservice.config;

/**
 * Created by @author mapuo on 16.10.17.
 */
public interface Configuration {

  Configuration DEFAULT = new Configuration() {
    // Nothing to implement, all methods should have default modifier
  };

  String SYSTEM_PROPERTY_KEY = "comms.router.config.file";

  default Integer getClientConnectTimeout() {
    return 1500;
  }

  default Integer getClientReadTimeout() {
    return 1500;
  }

  default Boolean getClientFollowRedirects() {
    return true;
  }

}
