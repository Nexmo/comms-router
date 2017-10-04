package com.softavail.comms.demo.application.api;

import com.nexmo.client.voice.CallDirection;
import com.nexmo.client.voice.CallStatus;
import com.nexmo.client.voice.ncco.TalkNcco;
import com.nexmo.client.voice.servlet.NccoResponse;
import com.nexmo.client.voice.servlet.NccoResponseBuilder;
import com.softavail.comms.demo.application.impl.NexMoConversationServiceImpl;
import com.softavail.comms.demo.application.model.ConversationNccoEx;
import com.softavail.comms.demo.application.model.NexMoCall;
import com.softavail.comms.demo.application.model.NexMoConversationStatus;
import com.softavail.comms.demo.application.model.UpdateNexMoConversationArg;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.ConversationService;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.LongAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.client.TaskServiceClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.MalformedURLException;
import java.net.URI;
import java.util.UUID;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;

@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Path("/answer_inbound")
public class NexMoAnswerInResource {

  private static final String queueId = "queue-demo";

  private static final Logger LOGGER = LogManager.getLogger(NexMoAnswerInResource.class);

  private ConversationService conversationService = new NexMoConversationServiceImpl();

  @Inject
  Configuration configuration;

  @Inject
  TaskServiceClient taskServiceClient;

  /**
   * .
   *
   * @param from query parameter from
   * @param to query parameter to
   * @param uuid query parameter uuid
   * @return JSON formatted response
   */
  @GET
  public String getAnswerNccoResponce(@QueryParam("from") String from, @QueryParam("to") String to,
      @QueryParam("conversation_uuid") String uuid)
      throws NotFoundException, MalformedURLException {

    LOGGER.debug("/answer_inbound with conversation_uuid: {}", uuid);

    NexMoCall call = conversationService.getInboundCallWithConversationId(uuid);
    if (null == call) {
      // create a call with uuid set as conv_uuid and later (on event started) we will update to the
      // real uuid
      NexMoCall newObj = new NexMoCall(uuid, uuid);
      newObj.setStatus(CallStatus.STARTED);
      newObj.setDirection(CallDirection.INBOUND);
      LOGGER.debug("[TEMP_CALL] ****** would create temp call: {}", newObj.getUuid());
      conversationService.updateCall(newObj);

      call = newObj.clone();
    }

    // Create new conversation into our service
    String conversationId = "conversation-" + UUID.randomUUID().toString();
    CreateTaskArg taskReq = new CreateTaskArg();
    RouterObjectId taskId =
        new RouterObjectId(UUID.randomUUID().toString(), configuration.getCommsRouterId());
    URI uri = UriBuilder.fromPath(configuration.getCallbackBaseUrl()).path("comms_callback")
        .path(taskId.getId()).queryParam("callId", conversationId).build();
    taskReq.setCallbackUrl(uri.toURL());
    taskReq.setQueueId(queueId);

    AttributeGroupDto requirements = new AttributeGroupDto();
    requirements.put("language", new StringAttributeValueDto("en"));
    requirements.put("color", new StringAttributeValueDto("red"));
    requirements.put("price", new LongAttributeValueDto(20));
    taskReq.setRequirements(requirements);

    TaskDto task = null;

    try {
      task = taskServiceClient.replace(taskReq, taskId);
    } catch (NotFoundException e) {
      e.printStackTrace();
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    if (null == task) {
      TalkNcco talkNccoErr = new TalkNcco("Sorry, we can't serve your request right now");
      talkNccoErr.setLoop(1);

      NccoResponseBuilder builder = new NccoResponseBuilder();
      builder.appendNcco(talkNccoErr);

      NccoResponse nccoResponse = builder.getValue();
      return nccoResponse.toJson();
    }

    // create a conversation for tracking
    conversationService.createConversation(conversationId, call, taskId.getId());
    UpdateNexMoConversationArg updateArg =
        new UpdateNexMoConversationArg(NexMoConversationStatus.WAITING);
    conversationService.updateConversation(conversationId, updateArg);

    // Response with NCCO
    TalkNcco talkNcco = new TalkNcco("Please wait while we connect you");
    talkNcco.setLoop(1);

    ConversationNccoEx convNcco = new ConversationNccoEx(conversationId);
    convNcco.setMusicOnHoldUrl(configuration.getMusicOnHoldUrl());
    convNcco.setStartOnEnter(false);
    convNcco.setRecord(false);

    NccoResponseBuilder builder = new NccoResponseBuilder();
    builder.appendNcco(talkNcco);
    builder.appendNcco(convNcco);

    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }

}
