package com.softavail.comms.demo.application.api;

import com.nexmo.client.voice.ncco.TalkNcco;
import com.nexmo.client.voice.servlet.NccoResponse;
import com.nexmo.client.voice.servlet.NccoResponseBuilder;
import com.softavail.comms.demo.application.impl.NexMoConversationServiceImpl;
import com.softavail.comms.demo.application.model.ConversationNccoEx;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.ConversationService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Path("/answer_outbound")
public class NexMoAnswerOutResource {

  private static final Logger LOGGER = LogManager.getLogger(NexMoAnswerOutResource.class);

  private ConversationService conversationService = new NexMoConversationServiceImpl();

  @Inject
  Configuration configuration;

  /**
   * .
   * @param uuid UUID
   * @return JSON
   */
  @GET
  @Path("/{conversationId}")
  public String getNccoResponse(
      @PathParam("conversationId") final String conversationId,
      @QueryParam("kind") String kind,
      @QueryParam("conversation_uuid") String uuid) {

    LOGGER.debug("/answer_outbound/{}", conversationId);
    
    String answer = "";

    //TODO: Check if the customer has left the conversation

    // Handle the answer
    if (kind != null && kind.equals("callback_agent")) {
      answer = handleAnswerFromAgentForCallbackTask(conversationId);
      // do not call the customer yet here
    } else if (kind != null && kind.equals("callback_customer")) {
      answer = handleAnswerFromCustomerForCallbackTask(conversationId);
    } else {
      answer = handleAnswerFromAgentForRegularTask(conversationId);
    }
    
    LOGGER.debug("/answer_outbound response:{}", answer);
    return answer;
  }

  
  private String handleAnswerFromAgentForCallbackTask(String conversationId) {
    
    Thread thread = new Thread(new Runnable() {
      
      @Override
      public void run() {
        // blah blah
      }
    }
    );
    
    thread.start();
    
    TalkNcco talkNcco = new TalkNcco("Please wait while we are connecting the the customer");
    talkNcco.setLoop(1);

    ConversationNccoEx convNcco = new ConversationNccoEx(conversationId);

    NccoResponseBuilder builder = new NccoResponseBuilder();
    builder.appendNcco(talkNcco);
    builder.appendNcco(convNcco);
    
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }
  
  private String handleAnswerFromCustomerForCallbackTask(String conversationId) {
    
    return null;
  }
  
  private String handleAnswerFromAgentForRegularTask(String conversationId) {
    TalkNcco talkNcco = new TalkNcco("Please wait while we connect you");
    talkNcco.setLoop(1);

    ConversationNccoEx convNcco = new ConversationNccoEx(conversationId);

    NccoResponseBuilder builder = new NccoResponseBuilder();
    builder.appendNcco(talkNcco);
    builder.appendNcco(convNcco);
    
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }

}
