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

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

/**
 * Created by @author mapuo on 16.10.17.
 */
@WebListener
public class WebServletListener implements ServletContextListener {

  private static final Logger LOGGER = LogManager.getLogger(WebServletListener.class);

  static final String APPLICATION_CONTEXT = "COMMS_ROUTER_APPLICATION_CONTEXT";

  private ApplicationContext applicationContext;

  @Override
  public void contextInitialized(ServletContextEvent sce) {
    applicationContext = new ApplicationContext(sce.getServletContext());
    sce.getServletContext().setAttribute(APPLICATION_CONTEXT, applicationContext);
  }

  @Override
  public void contextDestroyed(ServletContextEvent sce) {
    if (applicationContext != null) {
      applicationContext.close();
    }
  }

}
