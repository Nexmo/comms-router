package com.softavail.comms.demo.application.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Created by @author mapuo on 29.08.17.
 */
@Ignore
public class PropertiesConfigurationTest {

  private static ConfigurationProperties configuration;

  @BeforeClass
  public static void setUp() throws Exception {
    configuration = new PropertiesConfiguration();
  }

  @Test
  public void getCallbackBaseUrl() throws Exception {
    assertEquals("Url equals", "https://192.168.0.100/demo/", configuration.callbackBaseUrl());
  }

  @Test
  public void getPhone() throws Exception {
    assertEquals("Phone equals", "17072158889", configuration.phone());
  }

  @Test
  public void getAppId() throws Exception {
    assertEquals("App Id equals", "some-app-id", configuration.appId());
  }

  @Test
  public void getPrivateKey() throws Exception {
    assertNotNull("JWT is not null", configuration.appPrivateKey());
  }

}
