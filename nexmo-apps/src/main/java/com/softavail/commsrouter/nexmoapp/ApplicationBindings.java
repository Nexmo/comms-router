package com.softavail.commsrouter.nexmoapp;

import com.softavail.commsrouter.nexmoapp.api.service.ApplicationServiceImpl;
import com.softavail.commsrouter.nexmoapp.api.service.ExpressionServiceImpl;
import com.softavail.commsrouter.nexmoapp.api.service.SessionServiceImpl;
import com.softavail.commsrouter.nexmoapp.config.Configuration;
import com.softavail.commsrouter.nexmoapp.config.ConfigurationImpl;
import com.softavail.commsrouter.nexmoapp.interfaces.ApplicationService;
import com.softavail.commsrouter.nexmoapp.interfaces.ExpressionService;
import com.softavail.commsrouter.nexmoapp.interfaces.SessionService;
import com.softavail.commsrouter.providers.ClientFactory;
import org.glassfish.hk2.utilities.binding.AbstractBinder;

import javax.inject.Singleton;
import javax.ws.rs.client.Client;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ApplicationBindings extends AbstractBinder {

  @Override
  protected void configure() {

    bind(ConfigurationImpl.class)
        .to(Configuration.class)
        .in(Singleton.class);

    bindFactory(ClientFactory.class)
        .to(Client.class)
        .in(Singleton.class);

    bind(new ApplicationServiceImpl())
        .to(ApplicationService.class)
        .to(ApplicationServiceImpl.class);

    bind(new ExpressionServiceImpl())
        .to(ExpressionService.class)
        .to(ExpressionServiceImpl.class);

    bind(new SessionServiceImpl())
        .to(SessionService.class)
        .to(SessionServiceImpl.class);

  }

}
