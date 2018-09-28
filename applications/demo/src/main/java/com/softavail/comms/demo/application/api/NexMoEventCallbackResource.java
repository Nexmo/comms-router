package com.softavail.comms.demo.application.api;

import com.fasterxml.jackson.databind.JsonNode;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.nexmo.answer.AnswerStrategyException;
import com.softavail.comms.nexmo.answer.AnswerStrategyWithCallback;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

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
  public Response handleCallbackEvent(
      @QueryParam("taskId") String taskId,
      @QueryParam("callback_state") String callbackState,
      JsonNode payload) {

    LOGGER.debug("/event_callback task:{},state:{}", taskId, callbackState);
    LOGGER.debug("payload: {}", payload);
    
    Response response = null;

    try {
      String answerNcco = strategy.continueAnswerInboundCall(payload, taskId, callbackState);
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

  @POST
  @Path("connect_callback")
  public Response handleConnectCallbackEvent(
      @QueryParam("taskId") String taskId,
      @QueryParam("action") String action,
      JsonNode payload) {

    LOGGER.debug("/event_callback/connect_callback task:{}, action:{}", taskId, action);
    LOGGER.debug("payload: {}", payload);
    
    Response response = null;
    try {
      String answerNcco = strategy.continueAnswerOutboundCall(payload, taskId, action);
      LOGGER.debug("/event_callback/connect_callback ncco: {}", answerNcco);
      response = Response.ok(answerNcco, MediaType.APPLICATION_JSON).build();
    } catch (AnswerStrategyException e) {
      LOGGER.error("failed to handle connect_callback event {}", e.getMessage());
      response =  Response.status(Response.Status.INTERNAL_SERVER_ERROR)
          .entity(e.getMessage()).build();
    } catch (Exception ex) {
      LOGGER.error("failed to handle connect_callback event {}", ex.getMessage());
      response =  Response.status(Response.Status.INTERNAL_SERVER_ERROR)
          .entity(ex.getMessage()).build();
    }

    LOGGER.debug("/event_callback/connect_callback response: {}", response.toString());
    return response;
  }
  

}
