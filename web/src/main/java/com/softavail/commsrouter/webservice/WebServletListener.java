/*
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.commsrouter.webservice;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.shiro.web.env.EnvironmentLoader;

import java.util.EnumSet;
import javax.servlet.DispatcherType;
import javax.servlet.FilterRegistration;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

/**
 * Created by @author mapuo on 16.10.17.
 */
@WebListener
public class WebServletListener extends EnvironmentLoader implements ServletContextListener {

  private static final Logger LOGGER = LogManager.getLogger(WebServletListener.class);

  static final String APPLICATION_CONTEXT = "COMMS_ROUTER_APPLICATION_CONTEXT";

  private ApplicationContext applicationContext;

  @Override
  public void contextInitialized(ServletContextEvent sce) {
    LOGGER.debug("contextInitialized start");

    try {
      ServletContext sc = sce.getServletContext();

      applicationContext = new ApplicationContext(sc);
      sc.setAttribute(APPLICATION_CONTEXT, applicationContext);

      initializeShiro(sc, applicationContext);

    } catch (Exception e) {
      LOGGER.error("Exception: ", e);
    }

    LOGGER.debug("contextInitialized end");
  }

  @Override
  public void contextDestroyed(ServletContextEvent sce) {
    if (applicationContext != null) {
      applicationContext.close();
    }
  }


  private void initializeShiro(ServletContext sc, ApplicationContext appContext) {
    String shiroConfigLocations = appContext.getConfiguration().getShiroConfigLocations();

    sc.setInitParameter("shiroConfigLocations", shiroConfigLocations);

    FilterRegistration filterRegistration =
        sc.addFilter("ShiroFilter", "org.apache.shiro.web.servlet.ShiroFilter");
    filterRegistration.addMappingForUrlPatterns(EnumSet.of(DispatcherType.REQUEST,
        DispatcherType.FORWARD, DispatcherType.INCLUDE, DispatcherType.ERROR), true, "/*");

    initEnvironment(sc);
  }

}
