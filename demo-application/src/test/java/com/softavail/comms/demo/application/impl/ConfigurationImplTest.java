package com.softavail.comms.demo.application.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.softavail.comms.demo.application.services.Configuration;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * Created by @author mapuo on 30.08.17.
 */
@Ignore
@RunWith(Parameterized.class)
public class ConfigurationImplTest {

  @Parameters(name = "{index}: {0}")
  public static Object[] data() {
    return new Object[] {Cfg4jConfiguration.class, PropertiesConfiguration.class};
  }

  private Configuration configuration;

  public ConfigurationImplTest(Class<ConfigurationProperties> propertiesClass) throws Exception {

    ConfigurationProperties properties = propertiesClass.newInstance();
    configuration = new ConfigurationImpl(properties);
  }

  @Test
  public void getJwtAuthMethod() throws Exception {
    assertNotNull("JWT is not null", configuration.getJwtAuthMethod());
  }

  @Test
  public void getAssociatedPhone() throws Exception {
    assertEquals("Phone equals", "17072158889", configuration.getAssociatedPhone().toLog());
  }

  @Test
  public void getCallbackBaseUrl() throws Exception {
    assertEquals("Url equals", "https://192.168.0.100/demo/", configuration.getCallbackBaseUrl());
  }

}
