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
