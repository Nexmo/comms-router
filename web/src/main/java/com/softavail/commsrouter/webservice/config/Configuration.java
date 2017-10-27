package com.softavail.commsrouter.webservice.config;

/**
 * Created by @author mapuo on 16.10.17.
 */
public interface Configuration {

  String SYSTEM_PROPERTY_KEY = "comms.router.config.file";

  Integer getClientConnectTimeout();

  Integer getClientReadTimeout();

  Boolean getClientFollowRedirects();

  Integer getClientRetryDelaySeconds();

  Integer getClientRetryDelayMaxSeconds();

  Integer getClientRetryJitterMilliseconds();

  Integer getTaskDispatcherThreadPoolSize();

}
