package com.softavail.comms.demo.application;

import com.softavail.comms.demo.application.client.AgentServiceClient;
import com.softavail.comms.demo.application.client.PlanServiceClient;
import com.softavail.comms.demo.application.client.RouterServiceClient;
import com.softavail.comms.demo.application.client.TaskServiceClient;
import com.softavail.comms.demo.application.impl.Cfg4jConfiguration;
import com.softavail.comms.demo.application.impl.ClientFactory;
import com.softavail.comms.demo.application.impl.ConfigurationImpl;
import com.softavail.comms.demo.application.impl.ConfigurationProperties;
import com.softavail.comms.demo.application.impl.ExecutionFactory;
import com.softavail.comms.demo.application.impl.NexMoServiceImpl;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.NexMoService;
import com.softavail.comms.nexmo.answer.AnswerStrategy;
import com.softavail.comms.nexmo.answer.AnswerStrategyWithCallback;

import org.glassfish.hk2.utilities.binding.AbstractBinder;

import java.util.concurrent.ScheduledExecutorService;
import javax.inject.Singleton;
import javax.ws.rs.client.Client;
import com.softavail.commsrouter.api.interfaces.RouterService;
import com.softavail.commsrouter.api.interfaces.AgentService;
import com.softavail.commsrouter.api.interfaces.PlanService;
import com.softavail.commsrouter.api.interfaces.TaskService;

/**
 * Created by @author mapuo on 29.08.17.
 */
public class ApplicationBinder extends AbstractBinder {

  @Override
  protected void configure() {

    bindFactory(new ExecutionFactory())
        .to(ScheduledExecutorService.class)
        .in(Singleton.class);

    // bind(PropertiesConfiguration.class)
    bind(Cfg4jConfiguration.class)
        .to(ConfigurationProperties.class)
        .in(Singleton.class);

    bindFactory(new ClientFactory())
        .to(Client.class)
        .in(Singleton.class);

    bind(ConfigurationImpl.class)
        .to(Configuration.class)
        .in(Singleton.class);

    bind(NexMoServiceImpl.class)
        .to(NexMoService.class)
        .in(Singleton.class);

    bind(RouterServiceClient.class)
        .to(RouterService.class)
        .to(RouterServiceClient.class);

    bind(TaskServiceClient.class)
        .to(TaskService.class)
        .to(TaskServiceClient.class);

    bind(PlanServiceClient.class)
        .to(PlanService.class)
        .to(PlanServiceClient.class);

    bind(AgentServiceClient.class)
        .to(AgentService.class)
        .to(AgentServiceClient.class);
    
    bind(AnswerStrategyWithCallback.class)
        .to(AnswerStrategy.class)
        .to(AnswerStrategyWithCallback.class);

  }

}
