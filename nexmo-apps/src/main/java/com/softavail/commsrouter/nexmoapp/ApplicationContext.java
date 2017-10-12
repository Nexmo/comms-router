package com.softavail.commsrouter.nexmoapp;

import com.softavail.commsrouter.nexmoapp.api.service.PluginServiceImpl;
import com.softavail.commsrouter.nexmoapp.config.Configuration;
import com.softavail.commsrouter.nexmoapp.config.ConfigurationImpl;
import com.softavail.commsrouter.nexmoapp.interfaces.PluginService;
import com.softavail.commsrouter.nexmoapp.jpa.TransactionManagerFactory;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import javax.servlet.ServletContext;

/**
 * Created by @author mapuo on 13.10.17.
 */
public class ApplicationContext {

  public final Configuration configuration;
  public final PluginService pluginService;
  public final TransactionManagerFactory transactionManagerFactory;
  public final ScheduledExecutorService threadPoolExecutor;

  public ApplicationContext(final ServletContext servletContext) {
    configuration = new ConfigurationImpl(servletContext);
    pluginService = new PluginServiceImpl();
    threadPoolExecutor = new ScheduledThreadPoolExecutor(configuration.getThreadPoolSize());
    transactionManagerFactory = new TransactionManagerFactory();
  }

  public void close() {
    threadPoolExecutor.shutdown();
    transactionManagerFactory.close();
  }

}
