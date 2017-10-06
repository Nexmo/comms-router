package com.softavail.commsrouter.webservice;

import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.app.TaskDispatcher;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import com.softavail.commsrouter.webservice.impl.TaskEventHandlerImpl;
import com.softavail.commsrouter.webservice.providers.ClientFactory;
import io.swagger.jaxrs.config.BeanConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.glassfish.jersey.server.ResourceConfig;

import javax.annotation.PreDestroy;
import javax.ws.rs.ApplicationPath;
import javax.ws.rs.client.Client;

/**
 * Created by @author mapuo on 31.08.17.
 */
@ApplicationPath("/api")
public class CommsRouterApplication extends ResourceConfig {

  private static final Logger LOGGER = LogManager.getLogger(CommsRouterApplication.class);

  private final JpaDbFacade dbFacade;
  private final TaskDispatcher taskDispatcher;
  private final CommsRouterEvaluator evaluator;
  private final EntityMappers mappers;

  public CommsRouterApplication() {
    dbFacade = new JpaDbFacade();
    evaluator = new CommsRouterEvaluator();
    mappers = new EntityMappers();
    taskDispatcher = new TaskDispatcher(dbFacade, (taskAssignment) -> {
      ClientFactory clientFactory = new ClientFactory();
      Client client = clientFactory.provide();
      new TaskEventHandlerImpl(client, taskAssignment).handle();
      clientFactory.dispose(client);
    }, mappers);

    AppContext context = new AppContext(dbFacade, evaluator, taskDispatcher, mappers);

    register(new ApplicationBindings(context));

    packages(CommsRouterApplication.class.getPackage().getName());

    register(io.swagger.jaxrs.listing.ApiListingResource.class);
    register(io.swagger.jaxrs.listing.SwaggerSerializers.class);

    BeanConfig beanConfig = new BeanConfig();
    beanConfig.setSchemes(new String[] {"http"});
    beanConfig.setHost("localhost:8084");
    beanConfig.setBasePath("/api");
    beanConfig.setResourcePackage("com.softavail.commsrouter.webservice.resources");
    beanConfig.setScan(true);
    beanConfig.setPrettyPrint(true);

    LOGGER.debug("Application started!");
  }

  @PreDestroy
  public void destroy() {
    LOGGER.debug("Destroying...");
    taskDispatcher.close();
    dbFacade.close();
  }

}
