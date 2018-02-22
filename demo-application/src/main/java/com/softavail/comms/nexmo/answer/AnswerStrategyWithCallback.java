package com.softavail.comms.nexmo.answer;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeType;
import com.nexmo.client.NexmoClientException;
import com.nexmo.client.voice.Call;
import com.nexmo.client.voice.CallEvent;
import com.nexmo.client.voice.Endpoint;
import com.nexmo.client.voice.ncco.Ncco;
import com.nexmo.client.voice.ncco.TalkNcco;
import com.nexmo.client.voice.servlet.NccoResponse;
import com.nexmo.client.voice.servlet.NccoResponseBuilder;
import com.softavail.comms.demo.application.factory.NexMoModelFactory;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.NexMoService;
import com.softavail.comms.nexmo.ncco.NccoFactory;
import com.softavail.comms.nexmo.util.PhoneConverter;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfDoublesAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfStringsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueVisitor;
import com.softavail.commsrouter.api.dto.model.attribute.BooleanAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.client.TaskServiceClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.inject.Inject;
import javax.ws.rs.core.UriBuilder;

public class AnswerStrategyWithCallback implements AnswerStrategy {

  private static final Logger LOGGER = LogManager.getLogger(AnswerStrategyWithCallback.class);

  private static final String KEY_STATE = "callback_state";
  private static final String KEY_NUMBER = "callback_number";
  private static final String KEY_TEMP_NUMBER = "temp_number";
  private static final String KEY_CONV_NAME = "conv_name";
  private static final String KEY_KIND = "kind";
  private static final String KEY_RECORDING_URL = "recording_url";

  private static final String STATE_PROMPT_CALLBACK = "prompt_callback";
  private static final String STATE_PROMPT_CALLERID = "prompt_callerid";
  private static final String STATE_GET_NUMBER = "get_number";
  private static final String STATE_CONFIRM_NUMBER = "confirm_number";
  private static final String STATE_RECORD_NAME = "record_name";
  private static final String STATE_END = "completed";
  private static final String STATE_ASSIGNED = "assigned";

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

  private static final String PROMPT_RECORD_NAME_MESSAGE =
      "Please say your name followed by # after the beep";

  private static final String FINAL_MESSAGE =
      "Thank you. We will call you back when an agent is ready to handle your request";

  private static final String ERROR_MESSAGE_CRATE_TASK = "Sorry we can't serve your request";

  private static final String MESSAGE_REGULAR_TASK_GREETING = "Please wait while we connect you";

  private static final String MESSAGE_ASSIGNED_CALLBACK_TASK =
      "An agent become ready to handle your request. Please wait while we connect you";

  private NccoFactory nccoFactory = new NccoFactory();

  private TaskServiceClient taskServiceClient;

  private Configuration configuration;

  private NexMoService nexMoService;

  private boolean withFeatureRecordName;

  private Map<String, String> requirements;

  private Map<String, String> userContext;

  @Inject
  AnswerStrategyWithCallback(
      TaskServiceClient taskServiceClient,
      Configuration configuration,
      NexMoService nexMoService) {
    this.taskServiceClient =  taskServiceClient;
    this.configuration = configuration;
    this.nexMoService = nexMoService;
    this.withFeatureRecordName = false;
    this.requirements = null;
    this.userContext = null;
  }

  @Override
  public String answerInboundCallWithParams(
      Map<String, String> requirements, Map<String, String> userContext)
      throws AnswerStrategyException {

    if (requirements == null) {
      throw new AnswerStrategyException("Invalid argument: <requirements>");
    }

    this.requirements = requirements;
    this.userContext = userContext;

    LOGGER.debug("requirements: {}", this.requirements);
    LOGGER.debug("userContext: {}", this.userContext);

    String convUuid = requirements.get("conversation_uuid");
    String from = requirements.get("from");
    String to = requirements.get("to");
    String tag = null;
    if (userContext != null) {
      tag = userContext.get("customer_uuid");
    }
    return answerInboundCall(convUuid, from, to, tag);
  }

  @Override
  public String answerInboundCall(final String convUuid, final String from, final String to,
      String tag) throws AnswerStrategyException {

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
    CreatedTaskDto task = createRegularTask(conversationName, from, tag);

    if (null == task) {
      return respondWithErrorTalkNcco();
    }

    // Check answer time prediction
    if (task.getQueueTasks() < 1) {
      // replace regular task
      return respondWithRegularTask(conversationName);
    }

    if (task.getQueueTasks() < 3 ) {
      // prompt callback
      return respondByTransitionToPromptCallback(task.getRef());
    }

    // continue callback
    return respondWithCallbackTask(task.getRef(), from);
  }

  @Override
  public String continueAnswerInboundCall(JsonNode userInfo, String taskId, String state)
      throws AnswerStrategyException {

    LOGGER.debug("continueAnswerInboundCall");

    TaskDto task = getTask(taskId);
    if (null == task) {
      LOGGER.warn("Can't get task: {}", taskId);
      return respondWithErrorTalkNcco();
    }

    AttributeGroupDto taskContext =  task.getUserContext();
    if (null == taskContext) {
      LOGGER.warn("Task does not have a context: {}", taskId);
      return respondWithErrorTalkNcco();
    }

    // check if the task has been assigned during callback ivr
    if (task.getState() == TaskState.assigned) {
      return handleAssignedTask(task);
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
      case STATE_RECORD_NAME:
        nccoResponse = handleRecordNameResponse(userInfo, task);
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

  @Override
  public String answerOutboundCall(String kind, String taskId) throws AnswerStrategyException {

    String nccoResponse;

    // Handle the answer
    if (kind != null && kind.equals("callback_agent")) {
      nccoResponse = handleOutboundAnswerFromAgentForCallbackTask(taskId);
    } else if (kind != null && kind.equals("regular_agent")) {
      nccoResponse = handleOutboundAnswerFromAgentForRegularTask(taskId);
    }  else if (kind != null && kind.equals("callback_customer")) {
      nccoResponse = handleOutboundAnswerFromCustomerForCallbackTask(taskId);
    } else {
      nccoResponse = respondWithErrorTalkNcco();
    }

    return nccoResponse;
  }

  @Override
  public String continueAnswerOutboundCall(JsonNode userInfo, String taskId, String action)
      throws AnswerStrategyException {
    LOGGER.debug("continueAnswerOutboundCall");

    TaskDto task = getTask(taskId);
    if (null == task) {
      LOGGER.warn("Can't get task: {}", taskId);
      return respondWithErrorTalkNcco();
    }

    String nccoResponse = null;
    if (null != action && action.equals("confirm_name")) {
      nccoResponse = handleOutboundPromptCustomerName(userInfo, task);
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

  private String handleAssignedTask(TaskDto task) {

    LOGGER.debug("handleAssignedTask");

    try {

      if (task == null) {
        return respondWithErrorTalkNcco();
      }

      AttributeGroupDto taskContext = task.getUserContext();
      if (null == taskContext) {
        return respondWithErrorTalkNcco();
      }

      String convName = attributeGroupDtogetString(KEY_CONV_NAME, taskContext);
      if (null == convName || convName.length() == 0) {
        return respondWithErrorTalkNcco();
      }

      // update task
      AttributeGroupDto updContext = new AttributeGroupDto();
      updContext.put(KEY_STATE, new StringAttributeValueDto(STATE_ASSIGNED));
      updContext.put(KEY_KIND, new StringAttributeValueDto("callback"));
      boolean taskUpdated = updateTaskContext(task.getRef(), updContext);
      if (false == taskUpdated) {
        return respondWithErrorTalkNcco();
      }

      // connect customer to the conversation
      List<Ncco> list = nccoFactory
          .nccoListWithAnswerFromCustomerForCallbackTask(MESSAGE_ASSIGNED_CALLBACK_TASK, convName);

      // preparing a response
      NccoResponseBuilder builder = new NccoResponseBuilder();
      list.forEach(ncco -> {
        builder.appendNcco(ncco);
      });

      // respond
      NccoResponse nccoResponse = builder.getValue();
      return nccoResponse.toJson();
    } catch (Exception e) {
      LOGGER.error("respondByTransitionToRegularTask: {}", e.getMessage());
    }

    return respondWithErrorTalkNcco();
  }

  private String handlePromptCallerIdResponse(JsonNode userInfo, TaskDto task) {
    LOGGER.debug("handlePromptCallerIdResponse");
    String response = "[]";// empty NCCO
    String number = parseDtmfFromUserInfo(userInfo);
    if (number != null && number.equals("1")) {
      if (withFeatureRecordName) {
        response = respondByTransitionToRecordName(task, null);
      } else {
        response = respondByTransitionToEnd(task, null);
      }
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
      if (withFeatureRecordName) {
        response = respondByTransitionToRecordName(task, taskContext);
      } else {
        response = respondByTransitionToEnd(task, taskContext);
      }
    } else {
      response = respondByTransitionToGetNumber(task, null);
    }

    return response;
  }

  private String handleRecordNameResponse(JsonNode userInfo, TaskDto task) {
    LOGGER.debug("handleRecordNameResponse");
    String recordingUrl = parseRecordingUrlFromUserInfo(userInfo);
    AttributeGroupDto taskContext = null;

    if (recordingUrl != null) {
      taskContext = new AttributeGroupDto();
      taskContext.put(KEY_RECORDING_URL, new StringAttributeValueDto(recordingUrl));
    }

    return respondByTransitionToEnd(task, taskContext);
  }

  private String respondWithCallbackTask(String taskId, String callbackNumber) {

    TaskDto task = getTask(taskId);
    if (null == task) {
      return respondWithErrorTalkNcco();
    }

    //TODO: set task's kind to callback
    AttributeGroupDto taskContext = new AttributeGroupDto();
    taskContext.put(KEY_KIND, new StringAttributeValueDto("cllback"));

    boolean updateTaskContextResult = updateTaskContext(task.getRef(), taskContext);
    if (false == updateTaskContextResult) {
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
      List<Ncco> list = nccoFactory.nccoListWithAnswerFromCustomerForRegularTask(
          MESSAGE_REGULAR_TASK_GREETING, convName, getMusicOnHoldUrl());

      NccoResponseBuilder builder = new NccoResponseBuilder();
      list.forEach(ncco -> {
        builder.appendNcco(ncco);
      });

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

    boolean updateTaskContextResult = updateTaskContext(task.getRef(), taskContext);
    if (false == updateTaskContextResult) {
      respondWithErrorTalkNcco();
    }

    try {
      String convName = attributeGroupDtogetString(KEY_CONV_NAME, task.getUserContext());
      List<Ncco> list = nccoFactory.nccoListWithAnswerFromCustomerForRegularTask(
          MESSAGE_REGULAR_TASK_GREETING, convName, getMusicOnHoldUrl());

      NccoResponseBuilder builder = new NccoResponseBuilder();
      list.forEach(ncco -> {
        builder.appendNcco(ncco);
      });

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
    String taskId = task.getRef();
    String state = STATE_PROMPT_CALLERID;
    AttributeGroupDto taskContext = new AttributeGroupDto();
    taskContext.put(KEY_STATE, new StringAttributeValueDto(state));
    taskContext.put(KEY_KIND, new StringAttributeValueDto("callback"));
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
    String taskId = task.getRef();
    String state = STATE_GET_NUMBER;
    AttributeGroupDto taskContext = new AttributeGroupDto();
    taskContext.put(KEY_STATE, new StringAttributeValueDto(state));
    taskContext.put(KEY_KIND, new StringAttributeValueDto("callback"));
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
    String taskId = task.getRef();
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

  private String respondByTransitionToRecordName(TaskDto task, AttributeGroupDto moreContext) {
    LOGGER.debug("respondByTransitionToRecordName");
    String taskId = task.getRef();
    String state = STATE_RECORD_NAME;
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

    URI uri = UriBuilder.fromPath(getEventUrl())
        .queryParam("taskId", taskId)
        .queryParam(KEY_STATE, state)
        .build();
    String finalEventUrl = uri.toString();

    // preparing a response
    List<Ncco> list = nccoFactory.nccoListWithPromptRecordName(PROMPT_RECORD_NAME_MESSAGE,
        finalEventUrl, FINAL_MESSAGE);
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
    String taskId = task.getRef();
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

  private String handleOutboundAnswerFromAgentForCallbackTask(String taskId) {
    LOGGER.trace("handleOutboundAnswerFromAgentForCallbackTask");

    String answer = null;
    TaskDto task = getTask(taskId);

    if (null != task) {
      // call customer at number
      String callbackNumber = attributeGroupDtogetString("callback_number", task.getUserContext());
      if (null != callbackNumber) {
        String conversationName = attributeGroupDtogetString(KEY_CONV_NAME, task.getUserContext());
        if (null != conversationName) {

          String callbackState =  attributeGroupDtogetString(KEY_STATE, task.getUserContext());

          if (null != callbackState && callbackState.length() > 0) {
            boolean flagOk = false;
            // check the callback state and call customer only if it has completed its callback IVR
            if (callbackState.equals(STATE_END)) {
              LOGGER.trace("Will customer to connect it to the agent");
              flagOk = true;
              new Thread(new Runnable() {
                @Override
                public void run() {
                  callCustomer(callbackNumber, taskId);
                }
              }).start();
            } else if (callbackState.equals(STATE_ASSIGNED)) {
              LOGGER.trace("Will not call customer as the task has been assigned");
              flagOk = true;
            }

            if (flagOk) {
              String text = "Please wait while we are connecting to the customer";
              String musicOnHoldUrl = configuration.getMusicOnHoldUrl();
              List<Ncco> list = nccoFactory.nccoListWithAnswerFromAgentForCallbackTask(text,
                  conversationName, musicOnHoldUrl);

              // preparing a response
              NccoResponseBuilder builder = new NccoResponseBuilder();
              list.forEach(ncco -> {
                builder.appendNcco(ncco);
              });

              NccoResponse nccoResponse = builder.getValue();
              answer = nccoResponse.toJson();
            } else {
              LOGGER.trace("Customer's call has been left unfinished. Will not try to connect");
            }
          }
        }
      }
    }

    if (null != answer) {
      return answer;
    }

    return respondWithErrorTalkNcco();
  }

  private String handleOutboundAnswerFromCustomerForCallbackTask(String taskId) {
    LOGGER.trace("handleOutboundAnswerFromCustomerForCallbackTask");

    TaskDto task = getTask(taskId);

    if (null != task) {
      if (this.withFeatureRecordName) {
        String recordingUrl = attributeGroupDtogetString(KEY_RECORDING_URL, task.getUserContext());
        //TODO: check if string is valid URL
        if (null != recordingUrl && recordingUrl.length() > 0) {
          return respondOutboundByPromptCustomerName(task, recordingUrl);
        }
      }

      return respondOutboundByConnectCustomer(task);
    }

    return respondWithErrorTalkNcco();
  }

  private String handleOutboundAnswerFromAgentForRegularTask(String taskId) {

    LOGGER.trace("handleOutboundAnswerFromAgentForRegularTask");

    TaskDto task = getTask(taskId);

    if (null != task) {
      String conversationName = attributeGroupDtogetString(KEY_CONV_NAME, task.getUserContext());
      if (null != conversationName) {

        List<Ncco> list = nccoFactory.nccoListWithAnswerFromAgentForRegularTask(
            MESSAGE_REGULAR_TASK_GREETING, conversationName, getMusicOnHoldUrl());

        // preparing a response
        NccoResponseBuilder builder = new NccoResponseBuilder();
        list.forEach(ncco -> {
          builder.appendNcco(ncco);
        });

        NccoResponse nccoResponse = builder.getValue();
        return nccoResponse.toJson();
      }
    }

    return respondWithErrorTalkNcco();
  }

  private String handleOutboundPromptCustomerName(JsonNode userInfo, TaskDto task) {
    LOGGER.debug("handleOutboundPromptCustomerName");
    String response = "[]";// empty NCCO
    String number = parseDtmfFromUserInfo(userInfo);
    if (number != null && number.equals("1")) {
      response = respondOutboundByConnectCustomer(task);
    } else {
      // continue regular
      response = respondWithErrorTalkNcco();
    }

    return response;
  }

  private String respondOutboundByConnectCustomer(TaskDto task) {
    LOGGER.debug("respondOutboundByConnectCustomer");

    if (null != task && null != task.getUserContext()) {
      String conversationName = attributeGroupDtogetString(KEY_CONV_NAME, task.getUserContext());
      if (null != conversationName) {
        String text = "Please wait while we connect you";

        List<Ncco> list = nccoFactory.nccoListWithAnswerFromCustomerForCallbackTask(text,
            conversationName);

        // preparing a response
        NccoResponseBuilder builder = new NccoResponseBuilder();
        list.forEach(ncco -> {
          builder.appendNcco(ncco);
        });

        NccoResponse nccoResponse = builder.getValue();
        return nccoResponse.toJson();
      }
    }

    return respondWithErrorTalkNcco();
  }

  private String respondOutboundByPromptCustomerName(TaskDto task, String recordingUrl) {

    LOGGER.debug("respondOutboundByPromptCustomerName");

    if (null != task && null != task.getUserContext() && recordingUrl != null) {
      String conversationName = attributeGroupDtogetString(KEY_CONV_NAME, task.getUserContext());
      if (null != conversationName) {
        String text1 = "Hi! Is ";
        String text2 = " on the line? When ";
        String text3 = " is on the line, press 1";

        URI uri = UriBuilder.fromPath(getEventUrl())
            .path("connect_callback")
            .queryParam("taskId", task.getRef())
            .queryParam("action", "confirm_name")
            .build();
        String eventUrl = uri.toString();
        List<Ncco> list = nccoFactory.nccoListWithPromptCustomerNameForCallbackTask(recordingUrl,
            text1, text2, text3, eventUrl);

        // preparing a response
        NccoResponseBuilder builder = new NccoResponseBuilder();
        list.forEach(ncco -> {
          builder.appendNcco(ncco);
        });

        NccoResponse nccoResponse = builder.getValue();
        return nccoResponse.toJson();
      }
    }

    return respondWithErrorTalkNcco();
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

  private String parseRecordingUrlFromUserInfo(JsonNode userInfo) {

    if (userInfo.get("recording_url").getNodeType() == JsonNodeType.STRING) {
      String url = userInfo.get("recording_url").asText();
      return url;
    }

    return null;
  }

  private CreatedTaskDto createRegularTask(String conversationName, String from, String tag) {
    CreatedTaskDto task = null;

    try {
      CreateTaskArg taskReq = new CreateTaskArg();
      URI uri = UriBuilder.fromPath(getTaskCallbackUrl()).build();

      taskReq.setCallbackUrl(uri.toURL());

      AttributeGroupDto userContext = new AttributeGroupDto();
      userContext.put(KEY_CONV_NAME, new StringAttributeValueDto(conversationName));
      // basic check for valid phone number
      if (from != null && from.length() > 9 && Character.isDigit(from.charAt(1))) {
        userContext.put(KEY_NUMBER, new StringAttributeValueDto(from));
      }
      // add userContext if any
      if (this.userContext != null && this.userContext.size() > 0) {
        LOGGER.trace("Will create task with userContext");

        this.userContext.keySet().forEach(key -> {
          String value = this.userContext.get(key);
          userContext.put(key, new StringAttributeValueDto(value));
        });
      }
      taskReq.setUserContext(userContext);

      // add requirements if any
      if (this.requirements != null && this.requirements.size() > 0) {
        LOGGER.trace("Will create task with requirements");
        AttributeGroupDto requirements = new AttributeGroupDto();

        this.requirements.keySet().forEach(key -> {
          String value = this.requirements.get(key);
          requirements.put(key, new StringAttributeValueDto(value));
        });

        taskReq.setRequirements(requirements);
        taskReq.setPlanRef(getPlanId());
        if (tag != null) {
          taskReq.setTag(tag);
        }
      } else {
        taskReq.setQueueRef(getQueueId());
      }

      task = taskServiceClient.create(taskReq, getRouterId());
    } catch (CommsRouterException e) {
      LOGGER.error("createRegularTask failed: {}", e.getMessage());
    } catch (Exception ex) {
      LOGGER.error("createRegularTask failed: {}", ex.getMessage());
    }

    return task;
  }

  private boolean updateTaskContext(String taskRef, AttributeGroupDto taskContext) {
    boolean result = false;

    try {
      RouterObjectRef routerObjectId = new RouterObjectRef(taskRef, getRouterId());
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

  private TaskDto getTask(String taskRef) {
    TaskDto task = null;
    try {
      RouterObjectRef routerObjectId = new RouterObjectRef(taskRef, getRouterId());
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
    return configuration.getNexmoCallbackBaseUrl() + "event_callback";
  }

  private String getTaskCallbackUrl() {
    return configuration.getCallbackBaseUrl() + "comms_callback";
  }

  private String getRouterId() {
    return configuration.getCommsRouterId();
  }

  private String getQueueId() {
    return configuration.getCommsQueueId();
  }

  private String getPlanId() {
    return configuration.getCommsPlanId();
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
        public void handleDoubleValue(DoubleAttributeValueDto value) throws IOException {
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
        public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
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

  private boolean callCustomer(String number, String taskRef) {

    boolean flagOk = false;
    CallEvent callEvent = null;

    try {
      // obtain agent's endpoint to be called
      String toNumber = number;//PhoneConverter.normalize(number);
      String fromNumber = PhoneConverter.normalize(configuration.getAssociatedPhone().toLog());
      Endpoint epTo = NexMoModelFactory.createEndpoint(toNumber);
      Endpoint epFrom = NexMoModelFactory.createEndpoint(fromNumber);
      URI uri = UriBuilder.fromPath(configuration.getNexmoCallbackBaseUrl())
          .path("answer_outbound")
          .queryParam("kind", "callback_customer")
          .queryParam("taskId", taskRef)
          .build();

      String answerUrl = uri.toString();

      // prepare to start a call to the agent
      Call callRequest = new Call(epTo, epFrom, answerUrl);

      // set event url
      URI evturi =
          UriBuilder.fromPath(configuration.getNexmoCallbackBaseUrl())
          .path("event_outbound")
          .queryParam("kind", "callback_customer")
          .queryParam("taskId", taskRef)
          .build();

      String eventUrl = evturi.toString();
      callRequest.setEventUrl(eventUrl);

      // start a call to the agent
      LOGGER.debug("calling customer at: {}", epTo.toLog());
      callEvent = nexMoService.getVoiceClient().createCall(callRequest);
      if (callEvent != null && callEvent.getUuid() != null) {
        LOGGER.debug("uuid: {}", callEvent.getUuid());
        flagOk = true;
      }

    } catch (IOException | NexmoClientException e) {
      // Would not call agent. Mark the task as complete with error.
      LOGGER.error("Failed to make a call to customer with error: {}", e.getLocalizedMessage());
    } catch (Exception ex) {
      // Would not call agent. Mark the task as complete with error.
      LOGGER.error("Failed to make a call to customer with error: {}", ex.getLocalizedMessage());
    }

    try {
      if (flagOk) {
        AttributeGroupDto userContext = new AttributeGroupDto();
        UpdateTaskContext updateCtx = new UpdateTaskContext();
        userContext.put("customer_uuid", new StringAttributeValueDto(callEvent.getUuid()));
        updateCtx.setUserContext(userContext);
        taskServiceClient.updateContext(updateCtx,
            new RouterObjectRef(taskRef, configuration.getCommsRouterId()));
      } else {
        // complete the task
        LOGGER.debug("Failed to call customer, mark task as completed: {}", taskRef);
        UpdateTaskArg updateArg = new UpdateTaskArg();
        updateArg.setState(TaskState.completed);
        taskServiceClient.update(updateArg,
            new RouterObjectRef(taskRef, configuration.getCommsRouterId()));
      }
    } catch (BadValueException | NotFoundException e1) {
      LOGGER.error("Failed to update task context with error: {}", e1.getLocalizedMessage());
    } catch (Exception e2) {
      LOGGER.error("Failed to update task context with eror: {}", e2.getLocalizedMessage());
    }

    return flagOk;
  }
}
