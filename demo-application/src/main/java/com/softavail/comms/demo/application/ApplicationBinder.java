package com.softavail.comms.demo.application;

import com.softavail.comms.demo.application.factory.AgentServiceClientFactory;
import com.softavail.comms.demo.application.factory.ClientFactory;
import com.softavail.comms.demo.application.factory.ExecutionFactory;
import com.softavail.comms.demo.application.factory.PlanServiceClientFactory;
import com.softavail.comms.demo.application.factory.QueueServiceClientFactory;
import com.softavail.comms.demo.application.factory.RouterServiceClientFactory;
import com.softavail.comms.demo.application.factory.TaskServiceClientFactory;
import com.softavail.comms.demo.application.impl.Cfg4jConfiguration;
import com.softavail.comms.demo.application.impl.ConfigurationImpl;
import com.softavail.comms.demo.application.impl.ConfigurationProperties;
import com.softavail.comms.demo.application.impl.NexMoServiceImpl;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.NexMoService;
import com.softavail.comms.nexmo.answer.AnswerStrategy;
import com.softavail.comms.nexmo.answer.AnswerStrategyWithCallback;
import com.softavail.comms.nexmo.ivr.IvrStrategy;
import com.softavail.comms.nexmo.ivr.IvrStrategyWithSimpleFlow;
import com.softavail.commsrouter.api.interfaces.AgentService;
import com.softavail.commsrouter.api.interfaces.PlanService;
import com.softavail.commsrouter.api.interfaces.QueueService;
import com.softavail.commsrouter.api.interfaces.RouterService;
import com.softavail.commsrouter.api.interfaces.TaskService;
import com.softavail.commsrouter.client.AgentServiceClient;
import com.softavail.commsrouter.client.PlanServiceClient;
import com.softavail.commsrouter.client.QueueServiceClient;
import com.softavail.commsrouter.client.RouterServiceClient;
import com.softavail.commsrouter.client.TaskServiceClient;
import org.glassfish.hk2.utilities.binding.AbstractBinder;

import java.util.concurrent.ScheduledExecutorService;
import javax.inject.Singleton;
import javax.ws.rs.client.Client;

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

    bindFactory(RouterServiceClientFactory.class)
        .to(RouterService.class)
        .to(RouterServiceClient.class);

    bindFactory(TaskServiceClientFactory.class)
        .to(TaskService.class)
        .to(TaskServiceClient.class);

    bindFactory(PlanServiceClientFactory.class)
        .to(PlanService.class)
        .to(PlanServiceClient.class);

    bindFactory(AgentServiceClientFactory.class)
        .to(AgentService.class)
        .to(AgentServiceClient.class);
    
    bind(AnswerStrategyWithCallback.class)
        .to(AnswerStrategy.class)
        .to(AnswerStrategyWithCallback.class);

    bind(IvrStrategyWithSimpleFlow.class)
        .to(IvrStrategy.class)
        .to(IvrStrategyWithSimpleFlow.class);
    
    bindFactory(QueueServiceClientFactory.class)
        .to(QueueService.class)
        .to(QueueServiceClient.class);

  }

}
