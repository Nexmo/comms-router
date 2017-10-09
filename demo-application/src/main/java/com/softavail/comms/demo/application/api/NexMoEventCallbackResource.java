package com.softavail.comms.demo.application.api;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.fasterxml.jackson.databind.JsonNode;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.nexmo.answer.AnswerStrategyException;
import com.softavail.comms.nexmo.answer.AnswerStrategyWithCallback;

@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Path("/event_callback")
public class NexMoEventCallbackResource {

  private static final Logger LOGGER = LogManager.getLogger(NexMoEventCallbackResource.class);
  
  @Inject
  Configuration configuration;
  
  @Inject
  AnswerStrategyWithCallback strategy;

  @POST
  public Response handleCallbackEvent(@Context UriInfo uriInfo,
      JsonNode payload) {

    LOGGER.debug("/event_callback");
    LOGGER.debug("payload: {}", payload);
    LOGGER.debug("context: {}", uriInfo.getQueryParameters());
    
    Response response = null;
    
    
    try {
      MultivaluedMap<String, String> context = uriInfo.getQueryParameters();
      String answerNcco = strategy.continueAnswerInboundCall(payload, context);
      LOGGER.debug("/event_callback ncco: {}", answerNcco);
      response = Response.ok(answerNcco, MediaType.APPLICATION_JSON).build();
    } catch (AnswerStrategyException e) {
      LOGGER.error("failed to handle callback event {}", e.getMessage());
      response =  Response.status(Response.Status.INTERNAL_SERVER_ERROR)
          .entity(e.getMessage()).build();
    } catch (Exception ex) {
      LOGGER.error("failed to handle callback event {}", ex.getMessage());
      response =  Response.status(Response.Status.INTERNAL_SERVER_ERROR)
          .entity(ex.getMessage()).build();
    }

    LOGGER.debug("/event_callback response: {}", response.toString());
    return response;
  }
  
}
