package com.softavail.comms.demo.application.api;

import com.softavail.comms.nexmo.answer.AnswerStrategyException;
import com.softavail.comms.nexmo.answer.AnswerStrategyWithCallback;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Path("/answer_outbound")
public class NexMoAnswerOutResource {

  private static final Logger LOGGER = LogManager.getLogger(NexMoAnswerOutResource.class);

  @Inject
  AnswerStrategyWithCallback strategy;

  /**
   * .
   * @param kind String
   * @param taskId String
   * @return JSON
   */
  @GET
  public Response getNccoResponse(
      @QueryParam("kind") String kind,
      @QueryParam("taskId") String taskId) {

    LOGGER.debug("/answer_outbound kind:{}, taskId: {}",
        kind, taskId);

    Response response;
    
    try {
      String answerNcco = strategy.answerOutboundCall(kind, taskId);
      LOGGER.debug("/answer_outbound ncco: {}", answerNcco);
      response = Response.ok(answerNcco, MediaType.APPLICATION_JSON).build();
    } catch (AnswerStrategyException e) {
      LOGGER.error("/answer_outbound failed: {}", e.getMessage());
      response =  Response.status(Response.Status.BAD_REQUEST)
          .entity(e.getMessage()).build();
    } catch (Exception ex) {
      LOGGER.error("/answer_outbound failed: {}", ex.getMessage());
      response =  Response.status(Response.Status.INTERNAL_SERVER_ERROR)
          .entity(ex.getMessage()).build();
    }
    
    LOGGER.debug("/answer_outbound response: {}", response.toString());
    return response;
  }
  
}
