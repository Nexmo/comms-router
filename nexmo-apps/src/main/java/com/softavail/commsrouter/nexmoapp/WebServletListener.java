package com.softavail.commsrouter.nexmoapp;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

/**
 * Created by @author mapuo on 13.10.17.
 */
@WebListener
public class WebServletListener implements ServletContextListener {

  static final String APP_CONTEXT = "NEXMOAPPS_APP_CONTEXT";

  private ApplicationContext applicationContext;

  @Override
  public void contextInitialized(ServletContextEvent sce) {
    applicationContext = new ApplicationContext(sce.getServletContext());
    sce.getServletContext().setAttribute(APP_CONTEXT, applicationContext);
  }

  @Override
  public void contextDestroyed(ServletContextEvent sce) {
    if (applicationContext != null) {
      applicationContext.close();
    }
  }

}
