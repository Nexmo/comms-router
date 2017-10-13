package com.softavail.commsrouter.nexmoapp.api.callback;

import com.nexmo.client.voice.CallDirection;
import com.nexmo.client.voice.CallEvent;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.nexmoapp.config.Configuration;
import com.softavail.commsrouter.nexmoapp.domain.Module;
import com.softavail.commsrouter.nexmoapp.domain.Session;
import com.softavail.commsrouter.nexmoapp.domain.SessionReference;
import com.softavail.commsrouter.nexmoapp.domain.SessionReferenceKey;
import com.softavail.commsrouter.nexmoapp.domain.SessionReferenceKey.Type;
import com.softavail.commsrouter.nexmoapp.interfaces.PluginService;
import com.softavail.commsrouter.nexmoapp.interfaces.SessionReferenceService;
import com.softavail.commsrouter.nexmoapp.interfaces.SessionService;
import com.softavail.commsrouter.nexmoapp.jpa.TransactionManagerFactory;
import com.softavail.commsrouter.nexmoapp.plugin.NexmoCallEvent;
import com.softavail.commsrouter.nexmoapp.plugin.Plugin;
import com.softavail.commsrouter.nexmoapp.plugin.PluginContext;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

import java.util.concurrent.ScheduledExecutorService;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 * Created by @author mapuo on 09.10.17.
 */
@Api
@Path("{applicationId}/nexmo")
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class NexmoResource {

  @Inject
  private SessionService sessionService;

  @Inject
  private SessionReferenceService referenceService;

  @Inject
  private PluginService pluginService;

  @Inject
  private Configuration configuration;

  @Inject
  private ScheduledExecutorService executorService;

  @Inject
  private TransactionManagerFactory transactionManagerFactory;

  @GET
  @Path("inbound")
  @ApiOperation("Processes inbound calls from Nexmo")
  public Response answerInbound(
      @PathParam("applicationId") String applicationId,
      @QueryParam("from") String from,
      @QueryParam("to") String to,
      @QueryParam("conversation_uuid") String uuid) {

    // TODO Find the app

    return Response.ok()
        .build();
  }

  @GET
  @Path("outbound")
  @ApiOperation("Processes outbound calls from Nexmo")
  public Response answerOutbound(
      @PathParam("applicationId") String applicationId,
      @QueryParam("conversation_uuid") String uuid) {

    // TODO

    return Response.ok()
        .build();
  }

  @POST
  @Path("event")
  @ApiOperation("Processes call events from Nexmo")
  public Response event(
      @PathParam("applicationId") String applicationId,
      CallEvent callEvent)
      throws CommsRouterException {

    String callId = callEvent.getUuid();
    SessionReferenceKey key = new SessionReferenceKey(Type.call, callId);

    if (callEvent.getDirection() == CallDirection.INBOUND) {

      // TODO Get Application and Module
      Session session = new Session();
      sessionService.create(session);

      SessionReference reference = new SessionReference(key, session);
      referenceService.create(reference);

    } else {

      Session session = referenceService.getSessionByKey(key);
      Module module = session.getCurrentModule();
      Plugin plugin = pluginService.findByName(module.getProgram());
      final PluginContext context =
          new PluginContext(configuration, transactionManagerFactory, sessionService, session);
      final NexmoCallEvent nexmoEvent = new NexmoCallEvent(callEvent);
      // Execute plugin in new Thread
      executorService.execute(() ->
          plugin.handleEvent(context, nexmoEvent));

    }

    return Response.ok()
        .build();
  }

}
