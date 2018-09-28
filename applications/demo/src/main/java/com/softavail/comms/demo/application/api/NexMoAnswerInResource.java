package com.softavail.comms.demo.application.api;

import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.nexmo.answer.AnswerStrategyException;
import com.softavail.comms.nexmo.answer.AnswerStrategyWithCallback;
import com.softavail.comms.nexmo.ivr.IvrStrategyWithSimpleFlow;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.client.TaskServiceClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.MalformedURLException;
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
@Path("/answer_inbound")
public class NexMoAnswerInResource {

  private static final Logger LOGGER = LogManager.getLogger(NexMoAnswerInResource.class);

  @Inject
  Configuration configuration;

  @Inject
  TaskServiceClient taskServiceClient;
  
  @Inject
  IvrStrategyWithSimpleFlow strategy;
 
  /**
   * .
   *
   * @param from query parameter from
   * @param to query parameter to
   * @param uuid query parameter uuid
   * @return JSON formatted response
   */
  @GET
  public Response getAnswerNccoResponce(
      @QueryParam("from") String from, 
      @QueryParam("to") String to,
      @QueryParam("conversation_uuid") String uuid)
      throws NotFoundException, MalformedURLException {

    LOGGER.debug("/answer_inbound with convUuid: {}, from: {}, to: {}", uuid, from, to);
    Response response;
    
    try {
      String answerNcco = strategy.answerInboundCall(uuid, from, to);
      LOGGER.debug("/answer_inbound ncco: {}", answerNcco);
      response = Response.ok(answerNcco, MediaType.APPLICATION_JSON).build();
    } catch (Exception ex) {
      LOGGER.error("/answer_inbound failed: {}", ex.getMessage());
      response =  Response.status(Response.Status.INTERNAL_SERVER_ERROR)
          .entity(ex.getMessage()).build();
    }
    
    LOGGER.debug("/answer_inbound response: {}", response.toString());
    return response;

    /*
    if (null == uuid) {
      return Response.status(Response.Status.BAD_REQUEST)
          .entity("{\"error\":\"Missing param: <conversation_uuid>\"}").build();
    }
    
    if (null == from) {
      return Response.status(Response.Status.BAD_REQUEST)
          .entity("{\"error\":\"Missing param: <from>\"}").build();
    }

    if (null == to) {
      return Response.status(Response.Status.BAD_REQUEST)
          .entity("{\"error\":\"Missing param: <to>\"}").build();
    }
    */
    
    /*
    
    NexMoCall call = conversationService.getInboundCallWithConversationId(uuid);
    if (null == call) {
      // create a call with uuid set as conv_uuid and later (on event started) we will update to the
      // real uuid
      NexMoCall newObj = new NexMoCall(uuid, uuid);
      newObj.setStatus(NexMoCallStatus.STARTED);
      newObj.setDirection(NexMoCallDirection.INBOUND);
      LOGGER.debug("[TEMP_CALL] ****** would create temp call: {}", newObj.getUuid());
      conversationService.updateCall(newObj);

      call = newObj.clone();
    }

    // Create new conversation into our service
    String conversationId = "conversation-" + UUID.randomUUID().toString();
    CreateTaskArg taskReq = new CreateTaskArg();
    RouterObjectId taskId =
        new RouterObjectId(UUID.randomUUID().toString(), configuration.getCommsRouterId());
    URI uri = UriBuilder.fromPath(configuration.getCallbackBaseUrl())
        .path("comms_callback")
        .path(taskId.getId())
        .queryParam("callId", conversationId)
        .build();
    taskReq.setCallbackUrl(uri.toURL());
    taskReq.setQueueId(configuration.getCommsQueueId());

    AttributeGroupDto requirements = new AttributeGroupDto();
    requirements.put("language", new StringAttributeValueDto("en"));
    requirements.put("color", new StringAttributeValueDto("red"));
    requirements.put("price", new DoubleAttributeValueDto(20));
    taskReq.setRequirements(requirements);

    CreatedTaskDto task = null;

    try {
      task = taskServiceClient.create(taskReq, taskId);
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
      return Response.ok(nccoResponse.toJson(), MediaType.APPLICATION_JSON).build();
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
    return Response.ok(nccoResponse.toJson(), MediaType.APPLICATION_JSON).build();
    */
  }
}
