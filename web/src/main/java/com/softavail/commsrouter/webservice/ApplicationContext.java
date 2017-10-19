package com.softavail.commsrouter.webservice;

import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.exception.AssignmentRejectedException;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.app.TaskDispatcher;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import com.softavail.commsrouter.webservice.config.Configuration;
import com.softavail.commsrouter.webservice.config.ConfigurationImpl;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.logging.LoggingFeature;

import javax.servlet.ServletContext;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status.Family;

/**
 * Created by @author mapuo on 16.10.17.
 */
public class ApplicationContext {

  private final Client client;
  private final AppContext coreContext;
  private final Configuration configuration;

  public ApplicationContext(ServletContext servletContext) {
    configuration = new ConfigurationImpl(servletContext);
    client = createClient();

    JpaDbFacade db = new JpaDbFacade();
    CommsRouterEvaluator evaluator = new CommsRouterEvaluator();
    EntityMappers mappers = new EntityMappers();
    Integer poolSize = configuration.getTaskDispatcherThreadPoolSize();
    TaskDispatcher taskDispatcher =
        new TaskDispatcher(db, this::handleAssignment, mappers, poolSize);

    coreContext = new AppContext(db, evaluator, taskDispatcher, mappers);
  }

  public Client getClient() {
    return client;
  }

  public AppContext getCoreContext() {
    return coreContext;
  }

  private Client createClient() {
    ClientConfig config = new ClientConfig();
    config.property(ClientProperties.CONNECT_TIMEOUT, configuration.getClientConnectTimeout());
    config.property(ClientProperties.READ_TIMEOUT, configuration.getClientReadTimeout());
    config.register(new LoggingFeature());
    return ClientBuilder.newClient(config);
  }

  private void handleAssignment(TaskAssignmentDto taskAssignment)
      throws AssignmentRejectedException {

    String callbackUrl = taskAssignment.getTask().getCallbackUrl();

    Response response = client.target(callbackUrl)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .post(Entity.entity(taskAssignment, MediaType.APPLICATION_JSON_TYPE));

    if (response.getStatusInfo().getFamily().equals(Family.CLIENT_ERROR)) {
      String rejectionMessage = response.getStatusInfo().getReasonPhrase();
      throw new AssignmentRejectedException("Rejected with: " + rejectionMessage);
    }

  }

  public void close() {
    client.close();
    coreContext.taskDispatcher.close();
    coreContext.db.close();
  }

}
