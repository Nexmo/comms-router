package com.softavail.comms.demo.application.api;

import com.nexmo.client.NexmoClientException;
import com.nexmo.client.voice.Call;
import com.nexmo.client.voice.CallEvent;
import com.nexmo.client.voice.Endpoint;
import com.nexmo.client.voice.ncco.Ncco;
import com.nexmo.client.voice.ncco.TalkNcco;
import com.nexmo.client.voice.servlet.NccoResponse;
import com.nexmo.client.voice.servlet.NccoResponseBuilder;
import com.softavail.comms.demo.application.factory.NexMoModelFactory;
import com.softavail.comms.demo.application.impl.NexMoConversationServiceImpl;
import com.softavail.comms.demo.application.model.ConversationNccoEx;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.ConversationService;
import com.softavail.comms.demo.application.services.NexMoService;
import com.softavail.comms.nexmo.ncco.NccoFactory;
import com.softavail.comms.nexmo.util.PhoneConverter;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfBooleansAttributeValueDto;
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

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;

@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Path("/answer_outbound")
public class NexMoAnswerOutResource {

  private static final Logger LOGGER = LogManager.getLogger(NexMoAnswerOutResource.class);

  private NccoFactory nccoFactory = new NccoFactory(); 

  @Inject
  Configuration configuration;

  @Inject
  TaskServiceClient taskServiceClient;

  @Inject
  NexMoService nexMoService;

  /**
   * .
   * @param kind String
   * @param taskId String
   * @return JSON
   */
  @GET
  public String getNccoResponse(
      @QueryParam("kind") String kind,
      @QueryParam("taskId") String taskId) {

    LOGGER.debug("/answer_outbound kind:{}, taskId: {}",
        kind, taskId);
    
    String answer = "[]";

    //TODO: Check if the customer has left the conversation

    // Handle the answer
    if (kind != null && kind.equals("callback_agent")) {
      answer = handleAnswerFromAgentForCallbackTask(taskId);
    } else if (kind != null && kind.equals("regular_agent")) {
      answer = handleAnswerFromAgentForRegularTask(taskId);
    }  else if (kind != null && kind.equals("callback_customer")) {
      answer = handleAnswerFromCustomerForCallbackTask(taskId);
    } else {
      answer = respondWithError();
    }
    
    LOGGER.debug("/answer_outbound response:{}", answer);
    return answer;
  }

  
  private String handleAnswerFromAgentForCallbackTask(String taskId) {

    String answer = null;
    
    LOGGER.trace("handleAnswerFromAgentForCallbackTask");
    
    TaskDto task = getTask(taskId);
    
    if (null != task) {
      // call customer at number
      String callbackNumber = attributeGroupDtogetString("callback_number", task.getUserContext());
      if (null != callbackNumber) {
        String conversationName = attributeGroupDtogetString("conv_name", task.getUserContext());
        if (null != conversationName) {
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
          
          new Thread(new Runnable() {
            @Override
            public void run() {
              callCustomer(callbackNumber, taskId);
            }
          }).start();
        }
      }      
    }
    
    if (null != answer)
      return answer;
     
    return respondWithError();
  }
  
  private String handleAnswerFromCustomerForCallbackTask(String taskId) {
    LOGGER.trace("handleAnswerFromCustomerForCallbackTask");

    TaskDto task = getTask(taskId);
    
    if (null != task) {
      String conversationName = attributeGroupDtogetString("conv_name", task.getUserContext());
      if (null != conversationName) {

        String text = "Please wait while we connect you";
        String musicOnHoldUrl = configuration.getMusicOnHoldUrl();
        
        List<Ncco> list = nccoFactory.nccoListWithAnswerFromCustomerForCallbackTask(text,
            conversationName, musicOnHoldUrl);
        
        // preparing a response    
        NccoResponseBuilder builder = new NccoResponseBuilder();
        list.forEach(ncco -> {
          builder.appendNcco(ncco);
        });

        NccoResponse nccoResponse = builder.getValue();
        return nccoResponse.toJson();
      }
    }
    
    return "[]";
  }
  
  private String handleAnswerFromAgentForRegularTask(String taskId) {
    
    LOGGER.trace("handleAnswerFromAgentForRegularTask");
    
    TaskDto task = getTask(taskId);
    
    if (null != task) {
      String conversationName = attributeGroupDtogetString("conv_name", task.getUserContext());
      if (null != conversationName) {
        TalkNcco talkNcco = new TalkNcco("Please wait while we connect you");
        talkNcco.setLoop(1);

        ConversationNccoEx convNcco = new ConversationNccoEx(conversationName);

        NccoResponseBuilder builder = new NccoResponseBuilder();
        builder.appendNcco(talkNcco);
        builder.appendNcco(convNcco);
        
        NccoResponse nccoResponse = builder.getValue();
        return nccoResponse.toJson();
      }
    }

    return "[]";
  }

  private TaskDto getTask(String taskId) {
    TaskDto task = null;
    try {
      RouterObjectId routerObjectId = new RouterObjectId(taskId, configuration.getCommsRouterId());
      task = taskServiceClient.get(routerObjectId);
    } catch (CommsRouterException e) {
      e.printStackTrace();
    } catch (Exception ex) {
      ex.printStackTrace();
    }
    
    return task;
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

  private boolean callCustomer(String number, String taskId) {

    boolean flagOk = false;
    CallEvent callEvent = null;
    
    try {
      // obtain agent's endpoint to be called
      String toNumber = number;//PhoneConverter.normalize(number);
      String fromNumber = PhoneConverter.normalize(configuration.getAssociatedPhone().toLog());
      Endpoint epTo = NexMoModelFactory.createEndpoint(toNumber);
      Endpoint epFrom = NexMoModelFactory.createEndpoint(fromNumber);
      URI uri = UriBuilder.fromPath(configuration.getCallbackBaseUrl())
          .path("answer_outbound")
          .queryParam("kind", "callback_customer")
          .queryParam("taskId", taskId)
          .build();

      String answerUrl = uri.toString();

      // prepare to start a call to the agent
      Call callRequest = new Call(epTo, epFrom, answerUrl);

      // set event url
      URI evturi = 
          UriBuilder.fromPath(configuration.getCallbackBaseUrl())
          .path("event_outbound")
          .queryParam("kind", "callback_customer")
          .queryParam("taskId", taskId)
          .build();

      String eventUrl = evturi.toString();
      callRequest.setEventUrl(eventUrl);

      // start a call to the agent
      LOGGER.debug("calling customer at: {}", epTo.toLog());
      callEvent = nexMoService.getVoiceClient().createCall(callRequest);
      LOGGER.debug("uuid: {}", callEvent.getUuid());
      flagOk = true;
    
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
            new RouterObjectId(taskId, configuration.getCommsRouterId()));
      }
    } catch (BadValueException | NotFoundException e1) {
      LOGGER.error("Failed to update task context with error: {}", e1.getLocalizedMessage());
      e1.printStackTrace();
    } catch (Exception e2) {
      LOGGER.error("Failed to update task context with eror: {}", e2.getLocalizedMessage());
      e2.printStackTrace();
    }
    
    return flagOk;
  }

  private String respondWithError() {
    // preparing a response
    NccoResponseBuilder builder = new NccoResponseBuilder();
    Ncco talkNcco = new TalkNcco("Sorry, we can't handle your request");
    builder.appendNcco(talkNcco);

    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }
}
