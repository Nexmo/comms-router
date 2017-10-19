package com.softavail.commsrouter.nexmoapp;

import com.softavail.commsrouter.nexmoapp.api.service.ApplicationServiceImpl;
import com.softavail.commsrouter.nexmoapp.api.service.ModuleServiceImpl;
import com.softavail.commsrouter.nexmoapp.api.service.SessionReferenceServiceImpl;
import com.softavail.commsrouter.nexmoapp.api.service.SessionServiceImpl;
import com.softavail.commsrouter.nexmoapp.config.Configuration;
import com.softavail.commsrouter.nexmoapp.interfaces.ApplicationService;
import com.softavail.commsrouter.nexmoapp.interfaces.ModuleService;
import com.softavail.commsrouter.nexmoapp.interfaces.PluginService;
import com.softavail.commsrouter.nexmoapp.interfaces.SessionReferenceService;
import com.softavail.commsrouter.nexmoapp.interfaces.SessionService;
import com.softavail.commsrouter.nexmoapp.jpa.TransactionManagerFactory;
import com.softavail.commsrouter.providers.ClientFactory;
import org.glassfish.hk2.utilities.binding.AbstractBinder;

import java.util.concurrent.ScheduledExecutorService;
import javax.inject.Singleton;
import javax.ws.rs.client.Client;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ApplicationBindings extends AbstractBinder {

  private final ApplicationContext applicationContext;

  public ApplicationBindings(ApplicationContext applicationContext) {
    this.applicationContext = applicationContext;
  }

  @Override
  protected void configure() {

    bind(applicationContext.configuration)
        .to(Configuration.class);

    bind(applicationContext.transactionManagerFactory)
        .to(TransactionManagerFactory.class);

    bind(applicationContext.threadPoolExecutor)
        .to(ScheduledExecutorService.class);

    bind(applicationContext.pluginService)
        .to(PluginService.class);

    bindFactory(ClientFactory.class)
        .to(Client.class)
        .in(Singleton.class);

    bind(ApplicationServiceImpl.class)
        .to(ApplicationService.class);

    bind(ModuleServiceImpl.class)
        .to(ModuleService.class);

    bind(SessionServiceImpl.class)
        .to(SessionService.class);

    bind(SessionReferenceServiceImpl.class)
        .to(SessionReferenceService.class);

  }

}
