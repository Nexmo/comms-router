package com.softavail.commsrouter.webservice;

import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.exception.CallbackException;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.app.TaskDispatcher;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import com.softavail.commsrouter.jpa.JpaDbFacade;
import com.softavail.commsrouter.webservice.config.ConfigurationImpl;
import com.softavail.commsrouter.webservice.config.ManifestConfigurationImpl;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.logging.LoggingFeature;

import javax.servlet.ServletContext;
import javax.ws.rs.ProcessingException;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

/**
 * Created by @author mapuo on 16.10.17.
 */
public class ApplicationContext {

  private final Client client;
  private final AppContext coreContext;
  private final ConfigurationImpl configuration;
  private final ManifestConfigurationImpl manifest;

  public ApplicationContext(ServletContext servletContext) {
    configuration = new ConfigurationImpl(servletContext);
    manifest = new ManifestConfigurationImpl(servletContext);
    client = createClient();

    JpaDbFacade db = new JpaDbFacade(configuration);
    CommsRouterEvaluator evaluator = new CommsRouterEvaluator();
    EntityMappers mappers = new EntityMappers();
    TaskDispatcher taskDispatcher =
        new TaskDispatcher(db, mappers, configuration, this::handleAssignment);

    coreContext = new AppContext(db, evaluator, taskDispatcher, mappers);
  }

  public Client getClient() {
    return client;
  }

  public AppContext getCoreContext() {
    return coreContext;
  }

  public ConfigurationImpl getConfiguration() {
    return configuration;
  }

  public ManifestConfigurationImpl getManifest() {
    return manifest;
  }

  private Client createClient() {
    ClientConfig config = new ClientConfig();
    config.property(ClientProperties.CONNECT_TIMEOUT, configuration.getClientConnectTimeout());
    config.property(ClientProperties.READ_TIMEOUT, configuration.getClientReadTimeout());
    config.register(new LoggingFeature());
    return ClientBuilder.newClient(config);
  }

  private void handleAssignment(TaskAssignmentDto taskAssignment)
      throws CallbackException {

    try {
      String callbackUrl = taskAssignment.getTask().getCallbackUrl();

      Response response = client.target(callbackUrl)
          .property(ClientProperties.FOLLOW_REDIRECTS, configuration.getClientFollowRedirects())
          .request(MediaType.WILDCARD_TYPE)
          .post(Entity.entity(taskAssignment, MediaType.APPLICATION_JSON_TYPE));

      if (response.getStatus() == Status.SERVICE_UNAVAILABLE.getStatusCode()) {
        // On 503 response we will try again
        // TODO Retry-After header?!
        throw new CallbackException();
      }

    } catch (ProcessingException e) {
      throw new CallbackException();
    }
  }

  public void close() {
    client.close();
    coreContext.taskDispatcher.close();
    coreContext.db.close();
  }

}
