package com.softavail.comms.demo.application.api;

import com.nexmo.client.voice.ncco.TalkNcco;
import com.nexmo.client.voice.servlet.NccoResponse;
import com.nexmo.client.voice.servlet.NccoResponseBuilder;
import com.softavail.comms.demo.application.impl.NexMoConversationServiceImpl;
import com.softavail.comms.demo.application.model.ConversationNccoEx;
import com.softavail.comms.demo.application.model.NexMoCall;
import com.softavail.comms.demo.application.model.NexMoConversation;
import com.softavail.comms.demo.application.model.NexMoConversationStatus;
import com.softavail.comms.demo.application.model.UpdateNexMoConversationArg;
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
      @QueryParam("conversation_uuid") String uuid) {

    LOGGER.debug("/answer_outbound/{}", conversationId);
    
    NexMoCall call =  conversationService.getInboundCallWithConversationId(uuid);

    NexMoConversation conversation = conversationService.getConversation(conversationId);
    
    if (null == conversation || null == call) {
      TalkNcco talkNcco = new TalkNcco("Customer has left the conversation.");
      talkNcco.setLoop(1);
      NccoResponseBuilder builder = new NccoResponseBuilder();
      builder.appendNcco(talkNcco);

      NccoResponse nccoResponse = builder.getValue();
      LOGGER.debug("/answer_outbound response:{}", nccoResponse.toJson());
      return nccoResponse.toJson();
    }
    
    UpdateNexMoConversationArg updateArg =
        new UpdateNexMoConversationArg(NexMoConversationStatus.CONNECTED);
    conversationService.updateConversation(conversationId, updateArg);

    TalkNcco talkNcco = new TalkNcco("Please wait while we connect you");
    talkNcco.setLoop(1);

    ConversationNccoEx convNcco = new ConversationNccoEx(conversationId);

    NccoResponseBuilder builder = new NccoResponseBuilder();
    builder.appendNcco(talkNcco);
    builder.appendNcco(convNcco);
    
    NccoResponse nccoResponse = builder.getValue();
    LOGGER.debug("/answer_outbound response:{}", nccoResponse.toJson());
    return nccoResponse.toJson();
  }

}
