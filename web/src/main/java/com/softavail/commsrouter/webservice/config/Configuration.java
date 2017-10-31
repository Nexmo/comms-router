package com.softavail.commsrouter.webservice.config;

/**
 * Created by @author mapuo on 16.10.17.
 */
public interface Configuration {

  String SYSTEM_PROPERTY_KEY = "comms.router.config.file";

  Configuration DEFAULT = new Configuration() {
    @Override
    public Integer getClientConnectTimeout() {
      return 1500;
    }

    @Override
    public Integer getClientReadTimeout() {
      return 1500;
    }

    @Override
    public Boolean getClientFollowRedirects() {
      return true;
    }
  };


  Integer getClientConnectTimeout();

  Integer getClientReadTimeout();

  Boolean getClientFollowRedirects();

}
