package com.softavail.comms.demo.application.api;

import com.fasterxml.jackson.databind.JsonNode;
import com.softavail.comms.nexmo.ivr.IvrStrategyWithSimpleFlow;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
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

@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Path("/event_ivr")
public class NexMoEventIvrResource {
  
  private static final Logger LOGGER = LogManager.getLogger(NexMoEventIvrResource.class);
  
  @Inject
  IvrStrategyWithSimpleFlow strategy;
  
  @POST
  public Response handleIvrEvent(
      @Context UriInfo uriInfo,
      JsonNode payload) {

    StringBuilder builder = new StringBuilder();
    MultivaluedMap<String, String> params = uriInfo.getQueryParameters();
    
    if (params != null && params.size() > 0) {
      params.keySet().forEach(key -> {
        List<String> list = params.get(key);
        list.forEach(value -> {
          builder.append("&").append(key).append("=").append(value);
        });
      });
    }
    
    LOGGER.debug("/event_ivr{}", builder.toString());
    LOGGER.debug("payload: {}", payload);
    
    Response response = null;

    try {
      String answerNcco = strategy.continueAnswerInboundCall(payload, params);
      LOGGER.debug("/event_ivr ncco: {}", answerNcco);
      response = Response.ok(answerNcco, MediaType.APPLICATION_JSON).build();
    } catch (Exception ex) {
      LOGGER.error("failed to handle event_ivr {}", ex.getMessage());
      response =  Response.status(Response.Status.INTERNAL_SERVER_ERROR)
          .entity(ex.getMessage()).build();
    }

    LOGGER.debug("/event_ivr response: {}", response.toString());
    return response;
  }

}
