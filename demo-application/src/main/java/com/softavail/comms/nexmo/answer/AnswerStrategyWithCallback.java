package com.softavail.comms.nexmo.answer;

import java.net.URI;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.inject.Inject;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.UriBuilder;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeType;
import com.nexmo.client.voice.ncco.Ncco;
import com.nexmo.client.voice.ncco.TalkNcco;
import com.nexmo.client.voice.servlet.NccoResponse;
import com.nexmo.client.voice.servlet.NccoResponseBuilder;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.nexmo.ncco.NccoFactory;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.client.TaskServiceClient;

public class AnswerStrategyWithCallback implements AnswerStrategy {

  private static final Logger LOGGER = LogManager.getLogger(AnswerStrategyWithCallback.class);

  private static final String KEY_STATE = "xstate";
  private static final String KEY_NUMBER = "xnumber";

  private static final String STATE_OFFER_CALLBACK = "OFFER_CALLBACK";
  private static final String STATE_ASK_NUMBER = "ASK_NUMBER";
  private static final String STATE_RECORD_NAME = "RECORD_NAME";
  private static final String STATE_END = "END";

  private static final String OFFER_CALLBACK_QUESTION =
      "We are experiencing heavy load at this moment."
          + " Please press 1 if you want to call you back";

  private static final String GATHER_NUMBER_QUESTION =
      "Please leave your number followed by pound sign";
  
  private static final String RECORD_NAME_QUESTION =
      "Please say your name after the beep";

  private static final String FINAL_MESSAGE =
      "Thank you. We will call you back when an agent is ready to handle your request";
  
  private static final String ERROR_MESSAGE_CRATE_TASK = "Sorry we can't serve your request";
  
  private static final String MESSAGE_REGULAR_TASK_GREETING = "Please wait while we connect you";
  
  
  private NccoFactory nccoFactory = new NccoFactory(); 
  
  private TaskServiceClient taskServiceClient;

  private Configuration configuration;
  
  @Inject
  AnswerStrategyWithCallback(TaskServiceClient taskServiceClient, Configuration configuration) {
    this.taskServiceClient =  taskServiceClient;
    this.configuration = configuration;
  }
  
  @Override
  public String answerInboundCall(final String convUuid, final String from, final String to)
      throws AnswerStrategyException {

    if (null == convUuid) {
      throw new AnswerStrategyException("Missing param: <conversation_uuid>");
    }
    
    if (null == from) {
      throw new AnswerStrategyException("Missing param: <from>");
    }

    if (null == to) {
      throw new AnswerStrategyException("Missing param: <to>");
    }

    // TODO: Gather queue size
    
    URI uri = 
        UriBuilder.fromPath(getEventUrl())
        .queryParam(KEY_STATE, STATE_OFFER_CALLBACK)
        .queryParam("conversation_uuid", convUuid)
        .queryParam("from", from)
        .queryParam("to", to)
        .build();
    
    String finalEventUrl = uri.toString();
    List<Ncco> list = nccoFactory.nccoListWithOfferCallback(OFFER_CALLBACK_QUESTION, finalEventUrl);
    
    // preparing a response    
    NccoResponseBuilder builder = new NccoResponseBuilder();
    list.forEach(ncco -> {
      builder.appendNcco(ncco);
    });
    
    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }

  @Override
  public String continueAnswerInboundCall(final JsonNode userInfo,
      final MultivaluedMap<String, String> context) throws AnswerStrategyException {

    final String state = context.getFirst(KEY_STATE);
    String nccoResponse = null;
    final String number = parseDtmfFromUserInfo(userInfo);
    
    switch (state) {
      case STATE_OFFER_CALLBACK:
        if (null == number) {
          nccoResponse = processInboundCallByCreateRegularTask(context);
        } else {
          nccoResponse = processInboundCallInStateGatherNumber(context);
        }
        break;
      case STATE_ASK_NUMBER:
        nccoResponse = processInboundCallInStateEnd(number, context);
        break;
      default:
        nccoResponse = processInboundCallInStateEnd(number, context);
        break;
    }

    if (null == nccoResponse) {
      throw new AnswerStrategyException("Could not build next ncco");
    }
    
    return nccoResponse;
  }

  private String processInboundCallInStateGatherNumber(MultivaluedMap<String, String> context) {
    
    // build event URL with the state
    UriBuilder uriBuilder = UriBuilder.fromPath(getEventUrl());
    Iterator<String> it = context.keySet().iterator();
    while (it.hasNext()) {
      String key = it.next();
      String value = context.getFirst(key);
      if (key.equals(KEY_STATE)) {
        uriBuilder = uriBuilder.queryParam(key, STATE_ASK_NUMBER);
      } else {
        uriBuilder = uriBuilder.queryParam(key, value);
      }
    }

    String finalEventUrl = uriBuilder.build().toString();
    List<Ncco> list = nccoFactory.nccoListWithChangeNumber(GATHER_NUMBER_QUESTION, finalEventUrl);
    
    // preparing a response
    NccoResponseBuilder builder = new NccoResponseBuilder();
    list.forEach(ncco -> {
      builder.appendNcco(ncco);
    });

    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }

  /*
  private String processInboundCallInStateRecordName(final Map<String, String> context,
      final String eventUrl) {
    
    // preparing a response
    NccoResponseBuilder builder = new NccoResponseBuilder();
    Ncco talkNcco = nccoFactory.nccoTalkWithOfferCallback(RECORD_NAME_QUESTION);
    builder.appendNcco(talkNcco);

    // build event URL with the state
    UriBuilder uriBuilder = UriBuilder.fromPath(eventUrl);
    Iterator<String> it = context.keySet().iterator();
    while (it.hasNext()) {
      String key = it.next();
      String value = context.get(key);
      if (key.equals(KEY_STATE)) {
        uriBuilder = uriBuilder.queryParam(key, RECORD_NAME);
      } else {
        uriBuilder = uriBuilder.queryParam(key, value);
      }
    }

    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }*/

  private String processInboundCallInStateEnd(String number,
      MultivaluedMap<String, String> context) {

    HashMap<String, String> finalContext = new HashMap<String, String>();

    // build event URL with the state
    Iterator<String> it = context.keySet().iterator();
    while (it.hasNext()) {
      String key = it.next();
      String value = context.getFirst(key);
      if (false == key.equals(KEY_STATE)) {
        finalContext.put(key, value);
      }
    }

    if (null != number && number.length() > 0) {
      finalContext.put(KEY_NUMBER, number);
    }

    // Create a task in the router
    CreatedTaskDto task = createCallbackTask(finalContext);
    if (null == task ) {
      return respondWithErrorTalkNcco();
    }
    
    // preparing a response
    NccoResponseBuilder builder = new NccoResponseBuilder();
    Ncco talkNcco = nccoFactory.nccoTalkWithRegularTaskGreeting(FINAL_MESSAGE);
    builder.appendNcco(talkNcco);

    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }

  
  private String parseDtmfFromUserInfo(JsonNode userInfo) {

    if (userInfo.get("dtmf").getNodeType() == JsonNodeType.STRING) {
      String dtmf = userInfo.get("dtmf").asText();
      int indexOfPound = dtmf.indexOf("#"); 
      if ( indexOfPound != -1) {
        dtmf = dtmf.substring(0, indexOfPound);
      }
      
      return dtmf;
    }
    
    return null;
  }
  
  private String processInboundCallByCreateRegularTask(
      final MultivaluedMap<String, String> context) {

    String conversationId = "conv-" + UUID.randomUUID().toString();
    CreatedTaskDto task = createRegularTask(conversationId);
    
    if (null == task) {
      return respondWithErrorTalkNcco();
    }
    
    // Response with NCCO
    Ncco talkNcco = nccoFactory.nccoTalkWithRegularTaskGreeting(MESSAGE_REGULAR_TASK_GREETING);
    Ncco convNcco = nccoFactory.nccoConversationWithRegularTask(conversationId, getMusicOnHoldUrl());

    NccoResponseBuilder builder = new NccoResponseBuilder();
    builder.appendNcco(talkNcco);
    builder.appendNcco(convNcco);

    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }
  
  private CreatedTaskDto createCallbackTask(Map<String, String> context) {
    CreatedTaskDto task = null;

    try {
      CreateTaskArg taskReq = new CreateTaskArg();
      RouterObjectId taskId = new RouterObjectId(UUID.randomUUID().toString(), getRouterId());

      URI uri = UriBuilder.fromPath(getTaskCallbackUrl()).path(taskId.getId())
          .queryParam("kind", "callback").build();

      taskReq.setCallbackUrl(uri.toURL());
      taskReq.setQueueId(getQueueId());
      
      String conversationid = "conv-" + UUID.randomUUID().toString();
      AttributeGroupDto userContext = createUserContext(context);
      userContext.put("conversationid", new StringAttributeValueDto(conversationid));
      taskReq.setUserContext(userContext);

      if (null == taskServiceClient) {
        LOGGER.error("taskServiceClient = null");
      }

      task = taskServiceClient.create(taskReq, taskId);
    } catch (CommsRouterException e) {
      e.printStackTrace();
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    return task;
  }
  
  private CreatedTaskDto createRegularTask(final String conversationId) {
    CreatedTaskDto task = null;

    try {
      CreateTaskArg taskReq = new CreateTaskArg();
      RouterObjectId taskId = new RouterObjectId(UUID.randomUUID().toString(), getRouterId());

      URI uri = UriBuilder.fromPath(getTaskCallbackUrl())
          .path(taskId.getId())
          .queryParam("kind", "regular")
          .queryParam("callId", conversationId)
          .build();

      taskReq.setCallbackUrl(uri.toURL());
      taskReq.setQueueId(getQueueId());

      AttributeGroupDto userContext = new AttributeGroupDto(); 
      userContext.put("conversationid", new StringAttributeValueDto(conversationId));
      taskReq.setUserContext(userContext);

      task = taskServiceClient.create(taskReq, taskId);
    } catch (CommsRouterException e) {
      e.printStackTrace();
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    return task;
  }
  
  private AttributeGroupDto createUserContext(Map<String, String> context) {
    AttributeGroupDto userContext = new AttributeGroupDto();
    Iterator<String> it = context.keySet().iterator();
    
    while (it.hasNext()) {
      String key = it.next();
      userContext.put(key, new StringAttributeValueDto(context.get(key)));
    }
    
    return userContext;
  }
  
  
  private String respondWithErrorTalkNcco() {
    // preparing a response
    NccoResponseBuilder builder = new NccoResponseBuilder();
    Ncco talkNcco = new TalkNcco(ERROR_MESSAGE_CRATE_TASK);
    builder.appendNcco(talkNcco);

    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }
  
  private String getEventUrl() {
    return configuration.getCallbackBaseUrl() + "/event_callback";
  }
  
  private String getTaskCallbackUrl() {  
    return configuration.getCallbackBaseUrl() + "/comms_callback";
  }
  
  private String getRouterId() {  
    return configuration.getCommsRouterId();
  }

  private String getQueueId() {
    return configuration.getCommsQueueId();
  }

  private String getMusicOnHoldUrl() {
    return configuration.getMusicOnHoldUrl();
  }

}
