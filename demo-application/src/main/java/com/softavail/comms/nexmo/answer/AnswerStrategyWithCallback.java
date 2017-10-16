package com.softavail.comms.nexmo.answer;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
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
import com.softavail.comms.nexmo.util.PhoneConverter;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfBooleansAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfLongsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfStringsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueVisitor;
import com.softavail.commsrouter.api.dto.model.attribute.BooleanAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.LongAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.client.TaskServiceClient;

public class AnswerStrategyWithCallback implements AnswerStrategy {

  private static final Logger LOGGER = LogManager.getLogger(AnswerStrategyWithCallback.class);

  private static final String KEY_STATE = "callback_state";
  private static final String KEY_NUMBER = "callback_number";
  private static final String KEY_TEMP_NUMBER = "temp_number";
  private static final String KEY_CONV_NAME = "conv_name";
  private static final String KEY_KIND = "kind";
  
  private static final String STATE_PROMPT_CALLBACK = "prompt_callback";
  private static final String STATE_PROMPT_CALLERID = "prompt_callerid";
  private static final String STATE_GET_NUMBER = "get_number";
  private static final String STATE_CONFIRM_NUMBER = "confirm_number";
  private static final String STATE_END = "completed";

  private static final String INFORM_CALLBACK_MESSAGE =
      "We are experiencing a high call volume. "
      + "We will call you back when an agent becomes available.";
  
  private static final String PROMPT_CALLBACK_MESSAGE = 
      "We are experiencing a high call volume. "
      + "If you want us to call you back when an agent becomes available, press 1 now. "
      + "You will not lose your place in the queue. To continue waiting, press 2";

  private static final String PROMPT_CALLERID_MESSAGE = 
      "Is the best number we can reach you at %s? "
      + "To use this number, press 1 now. To give us another number, press 2 now.";
  
  private static final String GET_NUMBER_MESSAGE = 
      "Please enter your call back number followed by # now.";
  
  private static final String CONFIRM_NUMBER_MESSAGE =
      "If %s is the correct number, press 1 now, otherwise, press 2 now.";
  
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

    LOGGER.debug("answerInboundCall");
    
    if (null == convUuid) {
      throw new AnswerStrategyException("Missing param: <conversation_uuid>");
    }
    
    if (null == from) {
      throw new AnswerStrategyException("Missing param: <from>");
    }

    if (null == to) {
      throw new AnswerStrategyException("Missing param: <to>");
    }

    // Create task 
    String conversationName = "conv-" + UUID.randomUUID().toString();
    CreatedTaskDto task = createRegularTask(conversationName, from);

    if (null == task) {
      return respondWithErrorTalkNcco();
    }
   
    // Check answer time prediction
    if (task.getQueueTasks() < 1) {
      // create regular task
      return respondWithRegularTask(conversationName);
    } 
    
    if (task.getQueueTasks() < 3 ) {
      // prompt callback
      return respondByTransitionToPromptCallback(task.getId());
    }

    // continue callback
    return respondWithCallbackTask(task.getId(), from);
  }

  @Override
  public String continueAnswerInboundCall(JsonNode userInfo, String taskId, String state)
      throws AnswerStrategyException {

    LOGGER.debug("continueAnswerInboundCall");

    TaskDto task = getTask(taskId);
    if (null == task) {
      LOGGER.warn("Can't get task: {}", taskId);
      respondWithErrorTalkNcco();
    }

    AttributeGroupDto taskContext =  task.getUserContext();
    if (null == taskContext) {
      LOGGER.warn("Task does not have a context: {}", taskId);
      respondWithErrorTalkNcco();
    }
    
    String callbackState = stringAttributeValueDto(taskContext.get(KEY_STATE));
    LOGGER.trace("evt state: {}, task state: {}", state, callbackState);
    
    // TODO: Check the state from task and from callback
    
    String nccoResponse = null;
    
    switch (callbackState) {
      case STATE_PROMPT_CALLBACK:
        nccoResponse = handlePromptCallbackResponse(userInfo, task);
        break;
      case STATE_PROMPT_CALLERID:
        nccoResponse = handlePromptCallerIdResponse(userInfo, task);
        break;
      case STATE_GET_NUMBER:
        nccoResponse = handleGetNumberResponse(userInfo, task);
        break;
      case STATE_CONFIRM_NUMBER:
        nccoResponse = handleConfirmNumberResponse(userInfo, task);
        break;
      default:
        nccoResponse = respondWithErrorTalkNcco();
        break;
    }

    if (null == nccoResponse) {
      throw new AnswerStrategyException("Could not build next ncco");
    }
    
    return nccoResponse;
  }

  private String handlePromptCallbackResponse(JsonNode userInfo, TaskDto task) {
    LOGGER.debug("handlePromptCallbackResponse");
    String response = "[]";// empty NCCO
    String number = parseDtmfFromUserInfo(userInfo);
    if (number != null && number.equals("1")) {

      String callbackNumber = attributeGroupDtogetString(KEY_NUMBER, task.getUserContext());
      if (callbackNumber.length() > 9) {
        response = respondByTransitionToPromptCallerId(task, callbackNumber, null);
      } else {
        response = respondByTransitionToGetNumber(task, null);
      }
    } else {
      // continue regular
      response = respondByTransitionToRegularTask(task);
    }

    return response;
  }
  
  private String handlePromptCallerIdResponse(JsonNode userInfo, TaskDto task) {
    LOGGER.debug("handlePromptCallerIdResponse");
    String response = "[]";// empty NCCO
    String number = parseDtmfFromUserInfo(userInfo);
    if (number != null && number.equals("1")) {
      response = respondByTransitionToEnd(task, null);
    } else {
      response = respondByTransitionToGetNumber(task, null);
    }

    return response;
  }

  private String handleGetNumberResponse(JsonNode userInfo, TaskDto task) {
    LOGGER.debug("handleGetNumberResponse");
    String response = "[]";// empty NCCO
    String number = parseDtmfFromUserInfo(userInfo);

    if (number != null && number.length() > 9) {
      response = respondByTransitionToConfirmNumber(task, number);      
    } else {
      response = respondByTransitionToGetNumber(task, null);
    }

    return response;
  }
  
  private String handleConfirmNumberResponse(JsonNode userInfo, TaskDto task) {
    LOGGER.debug("handleConfirmNumberResponse");
    String response = "[]";// empty NCCO
    String dtmf = parseDtmfFromUserInfo(userInfo);

    if (dtmf != null && dtmf.equals("1")) {
      String number = attributeGroupDtogetString(KEY_TEMP_NUMBER, task.getUserContext());
      AttributeGroupDto taskContext = new AttributeGroupDto();
      taskContext.put(KEY_NUMBER, new StringAttributeValueDto(number));
      response = respondByTransitionToEnd(task, taskContext);
    } else {
      response = respondByTransitionToGetNumber(task, null);
    }

    return response;
  }

  private String respondWithCallbackTask(String taskId, String callbackNumber) {
    
    TaskDto task = getTask(taskId);
    if (null == task) {
      return respondWithErrorTalkNcco();
    }
    
    if (callbackNumber != null && callbackNumber.length() > 9
        && Character.isDigit(callbackNumber.charAt(1))) {
      return respondByTransitionToPromptCallerId(task, callbackNumber, INFORM_CALLBACK_MESSAGE);
    }
    
    return respondByTransitionToGetNumber(task, INFORM_CALLBACK_MESSAGE);
  }
  
  private String respondByTransitionToPromptCallback(String taskId) {
    
    AttributeGroupDto taskContext = new AttributeGroupDto();
    taskContext.put(KEY_STATE, new StringAttributeValueDto(STATE_PROMPT_CALLBACK));
    
    LOGGER.debug("will updateTaskContext with 'callback_state': {}", 
        STATE_PROMPT_CALLBACK);
    boolean taskUpdated = updateTaskContext(taskId, taskContext);
    
    if (false == taskUpdated) {
      return respondWithErrorTalkNcco();
    }
    
    URI uri = UriBuilder.fromPath(getEventUrl())
        .queryParam("taskId", taskId)
        .queryParam(KEY_STATE, STATE_PROMPT_CALLBACK)
        .build();
    String finalEventUrl = uri.toString();
    List<Ncco> list =
        nccoFactory.nccoListWithPromptCallback(PROMPT_CALLBACK_MESSAGE, finalEventUrl);
    
    // preparing a response    
    NccoResponseBuilder builder = new NccoResponseBuilder();
    list.forEach(ncco -> {
      builder.appendNcco(ncco);
    });
    
    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }
  
  private String respondWithRegularTask(String convName) {
    LOGGER.debug("respondWithRegularTask");

    try {
      Ncco talkNcco = nccoFactory.nccoTalkWithRegularTaskGreeting(MESSAGE_REGULAR_TASK_GREETING);
      Ncco convNcco = nccoFactory.nccoConversationWithRegularTask(convName, getMusicOnHoldUrl());

      NccoResponseBuilder builder = new NccoResponseBuilder();
      builder.appendNcco(talkNcco);
      builder.appendNcco(convNcco);

      NccoResponse nccoResponse = builder.getValue();
      return nccoResponse.toJson();
    } catch (Exception e) {
      LOGGER.error("respondByTransitionToRegularTask: {}", e.getMessage());
    }

    return respondWithErrorTalkNcco();
  }

  private String respondByTransitionToRegularTask(TaskDto task) {
    LOGGER.debug("respondByTransitionToRegularTask");
    
    AttributeGroupDto taskContext = new AttributeGroupDto();
    taskContext.put(KEY_KIND, new StringAttributeValueDto("regular"));
    
    boolean updateTaskContextResult = updateTaskContext(task.getId(), taskContext);
    if (false == updateTaskContextResult) {
      respondWithErrorTalkNcco();
    }

    try {
      String convName = attributeGroupDtogetString(KEY_CONV_NAME, task.getUserContext());
      Ncco talkNcco = nccoFactory.nccoTalkWithRegularTaskGreeting(MESSAGE_REGULAR_TASK_GREETING);
      Ncco convNcco = nccoFactory.nccoConversationWithRegularTask(convName, getMusicOnHoldUrl());

      NccoResponseBuilder builder = new NccoResponseBuilder();
      builder.appendNcco(talkNcco);
      builder.appendNcco(convNcco);

      NccoResponse nccoResponse = builder.getValue();
      return nccoResponse.toJson();
    } catch (Exception e) {
      LOGGER.error("respondByTransitionToRegularTask: {}", e.getMessage());
    }

    return respondWithErrorTalkNcco();
  }
  
  private String respondByTransitionToPromptCallerId(TaskDto task, String callerId,
      String callbackText) {
    LOGGER.debug("respondByTransitionToPromptCallerId");
    String taskId = task.getId();
    String state = STATE_PROMPT_CALLERID;
    AttributeGroupDto taskContext = new AttributeGroupDto();
    taskContext.put(KEY_STATE, new StringAttributeValueDto(state));
    boolean taskUpdated = updateTaskContext(taskId, taskContext);
    
    if (false == taskUpdated) {
      return respondWithErrorTalkNcco();
    }
    
    URI uri = UriBuilder.fromPath(getEventUrl())
        .queryParam("taskId", taskId)
        .queryParam(KEY_STATE, state)
        .build();
    String finalEventUrl = uri.toString();
    String machineReadableCallerId = PhoneConverter.machineReadable(callerId);
    String text = String.format(PROMPT_CALLERID_MESSAGE, machineReadableCallerId);
    String finalText = null; 

    if (callbackText != null && callbackText.length() > 0) {
      finalText = new StringBuilder().append(callbackText).append(text).toString();
    } else {
      finalText = text;
    }
    
    List<Ncco> list = nccoFactory.nccoListWithPromptCallerId(finalText, finalEventUrl);
    
    // preparing a response    
    NccoResponseBuilder builder = new NccoResponseBuilder();
    list.forEach(ncco -> {
      builder.appendNcco(ncco);
    });
    
    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }

  private String respondByTransitionToGetNumber(TaskDto task, String callbackText) {
    LOGGER.debug("respondByTransitionToGetNumber");
    String taskId = task.getId();
    String state = STATE_GET_NUMBER;
    AttributeGroupDto taskContext = new AttributeGroupDto();
    taskContext.put(KEY_STATE, new StringAttributeValueDto(state));
    boolean taskUpdated = updateTaskContext(taskId, taskContext);
    
    if (false == taskUpdated) {
      return respondWithErrorTalkNcco();
    }
    
    URI uri = UriBuilder.fromPath(getEventUrl())
        .queryParam("taskId", taskId)
        .queryParam(KEY_STATE, state)
        .build();
    String finalEventUrl = uri.toString();
    String finalText = null; 

    if (callbackText != null && callbackText.length() > 0) {
      finalText = new StringBuilder().append(callbackText).append(GET_NUMBER_MESSAGE).toString();
    } else {
      finalText = GET_NUMBER_MESSAGE;
    }

    List<Ncco> list = nccoFactory.nccoListWithGetNumber(finalText, finalEventUrl);
    
    // preparing a response    
    NccoResponseBuilder builder = new NccoResponseBuilder();
    list.forEach(ncco -> {
      builder.appendNcco(ncco);
    });
    
    
    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }

  private String respondByTransitionToConfirmNumber(TaskDto task, String number) {
    LOGGER.debug("respondByTransitionToConfirmNumber");
    String taskId = task.getId();
    String state = STATE_CONFIRM_NUMBER;
    AttributeGroupDto taskContext = new AttributeGroupDto();
    taskContext.put(KEY_STATE, new StringAttributeValueDto(state));
    taskContext.put(KEY_TEMP_NUMBER, new StringAttributeValueDto(number));
    boolean taskUpdated = updateTaskContext(taskId, taskContext);
    
    if (false == taskUpdated) {
      return respondWithErrorTalkNcco();
    }
    
    URI uri = UriBuilder.fromPath(getEventUrl())
        .queryParam("taskId", taskId)
        .queryParam(KEY_STATE, state)
        .build();
    String finalEventUrl = uri.toString();
    String machineReadableNumber = PhoneConverter.machineReadable(number);
    String confirmationMessage = String.format(CONFIRM_NUMBER_MESSAGE, machineReadableNumber);
    List<Ncco> list = nccoFactory.nccoListWithConfirmNumber(confirmationMessage, finalEventUrl);
    
    // preparing a response    
    NccoResponseBuilder builder = new NccoResponseBuilder();
    list.forEach(ncco -> {
      builder.appendNcco(ncco);
    });
    
    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }
  
  private String respondByTransitionToEnd(TaskDto task, AttributeGroupDto moreContext) {
    LOGGER.debug("respondByTransitionToEnd");
    String taskId = task.getId();
    String state = STATE_END;
    AttributeGroupDto taskContext = new AttributeGroupDto();
    taskContext.put(KEY_STATE, new StringAttributeValueDto(state));
    
    if (null != moreContext) {
      moreContext.keySet().forEach(key -> {
        taskContext.put(key, moreContext.get(key));
      });
    }
    
    boolean taskUpdated = updateTaskContext(taskId, taskContext);
    
    if (false == taskUpdated) {
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
  
  private CreatedTaskDto createRegularTask(String conversationName, String from) {
    CreatedTaskDto task = null;

    try {
      CreateTaskArg taskReq = new CreateTaskArg();
      URI uri = UriBuilder.fromPath(getTaskCallbackUrl()).build();

      taskReq.setCallbackUrl(uri.toURL());
      taskReq.setQueueId(getQueueId());

      AttributeGroupDto userContext = new AttributeGroupDto(); 
      userContext.put(KEY_CONV_NAME, new StringAttributeValueDto(conversationName));
      // basic check for valid phone number
      if (from != null && from.length() > 9 && Character.isDigit(from.charAt(1))) {
        userContext.put(KEY_NUMBER, new StringAttributeValueDto(from));
      }
      taskReq.setUserContext(userContext);

      task = taskServiceClient.create(taskReq, getRouterId());
    } catch (CommsRouterException e) {
      LOGGER.error("createRegularTask failed: {}", e.getMessage());
    } catch (Exception ex) {
      LOGGER.error("createRegularTask failed: {}", ex.getMessage());
    }

    return task;
  }
  
  private boolean updateTaskContext(String taskId, AttributeGroupDto taskContext) {
    boolean result = false;
    
    try {
      RouterObjectId routerObjectId = new RouterObjectId(taskId, getRouterId());
      UpdateTaskContext updTaskContext = new UpdateTaskContext();
      updTaskContext.setUserContext(taskContext);
      taskServiceClient.updateContext(updTaskContext, routerObjectId);
      result = true;
    } catch (CommsRouterException e) {
      LOGGER.error("updateTaskContext failed: {}", e.getMessage());
    } catch (Exception ex) {
      LOGGER.error("updateTaskContext failed: {}", ex.getMessage());
    }
    
    return result;
  }
  
  private TaskDto getTask(String taskId) {
    TaskDto task = null;
    try {
      RouterObjectId routerObjectId = new RouterObjectId(taskId, getRouterId());
      task = taskServiceClient.get(routerObjectId);
      
      LOGGER.trace("task:{}", task);
    } catch (CommsRouterException e) {
      e.printStackTrace();
    } catch (Exception ex) {
      ex.printStackTrace();
    }
    
    return task;
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

  private String stringAttributeValueDto(AttributeValueDto valueDto) {

    if (null == valueDto) {
      return "";
    }
    
    String stringValue = null;
    ArrayList<String> result = new ArrayList<String>();
    
    try {
      
      valueDto.accept( new AttributeValueVisitor() {
        @Override
        public void handleStringValue(StringAttributeValueDto value) throws IOException {
          result.add(value.getValue());
        }
        
        @Override
        public void handleLongValue(LongAttributeValueDto value) throws IOException {
          // TODO Auto-generated method stub
          
        }
        
        @Override
        public void handleBooleanValue(BooleanAttributeValueDto value) throws IOException {
          // TODO Auto-generated method stub
          
        }
        
        @Override
        public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value)
            throws IOException {
          // TODO Auto-generated method stub

        }
        
        @Override
        public void handleArrayOfLongsValue(ArrayOfLongsAttributeValueDto value)
            throws IOException {
          // TODO Auto-generated method stub

        }

        @Override
        public void handleArrayOfBooleansValue(ArrayOfBooleansAttributeValueDto value)
            throws IOException {
          // TODO Auto-generated method stub
          
        }
      });
    } catch (IOException e) {
      LOGGER.error(e.getLocalizedMessage());
    }
    
    if (result != null && result.size() > 0) {
      stringValue = result.get(0);
    }
    
    return stringValue;
  }

  private String attributeGroupDtogetString(String key, AttributeGroupDto groupDto) {
    try {
      return stringAttributeValueDto(groupDto.get(key));
    } catch (Exception e) {
      LOGGER.error("attributeGroupDtogetString: {}", e.getMessage());
    }
    return "";
  }
}
