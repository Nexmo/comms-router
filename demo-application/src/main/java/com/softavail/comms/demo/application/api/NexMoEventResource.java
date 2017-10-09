package com.softavail.comms.demo.application.api;

import com.nexmo.client.NexmoClientException;
import com.nexmo.client.voice.Call;
import com.nexmo.client.voice.CallDirection;
import com.nexmo.client.voice.CallEvent;
import com.nexmo.client.voice.Endpoint;
import com.softavail.comms.demo.application.factory.NexMoModelFactory;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.NexMoService;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfBooleansAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfLongsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfStringsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueVisitor;
import com.softavail.commsrouter.api.dto.model.attribute.BooleanAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.LongAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.client.TaskServiceClient;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;

@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Path("/event")
public class NexMoEventResource {

  private static final Logger LOGGER = LogManager.getLogger(NexMoEventResource.class);

  @Inject
  Configuration configuration;

  @Inject
  TaskServiceClient taskServiceClient;

  @Inject
  NexMoService nexMoService;

  @POST
  public Response callState(
      @QueryParam("kind") String kind,
      @QueryParam("taskId") String taskId,
      @QueryParam("callId") String conversationId,
      CallEvent callEvent) {
    
    if (callEvent != null) {
      LOGGER.debug("/event with call uuid: {} status: {} direction: {}",
          callEvent.getUuid(), callEvent.getStatus(), callEvent.getDirection());

      if (kind != null && kind.equals("callback_agent")) {
        handleAgentCallEvent(callEvent, taskId, kind, conversationId);
      } else if (kind != null && kind.equals("callback_customer")) {
        handleCustomerCallEvent(callEvent, taskId);
      } else if (callEvent.getDirection() == CallDirection.INBOUND) {
        handleCustomerCallEvent(callEvent, taskId);
      } else {
        handleAgentCallEvent(callEvent, taskId, kind, conversationId);
      }
    }

    Response response = Response.ok().build();
    return response;
  }

  private void handleCustomerCallEvent(CallEvent callEvent, String taskId) {
    LOGGER.trace("handleCustomerCallEvent");
    
    switch (callEvent.getStatus()) {
      case STARTED:
        break;
      case RINGING:
        break;
      case ANSWERED:
        handleCustomerAnsweredCallEvent(callEvent, taskId);
        break;
      case TIMEOUT:
        break;
      case MACHINE:
        break;
      case COMPLETED:
        handleCustomerCompletedCallEvent(callEvent, taskId);
        break;
      default:
        break;
    }
  }
  
  private void handleAgentCallEvent(CallEvent callEvent, String taskId, String kind,
      String conversationId) {
    LOGGER.trace("handleAgentCallEvent");
    switch (callEvent.getStatus()) {
      case STARTED:
        break;
      case RINGING:
        break;
      case ANSWERED:
        handleAgentAnsweredCallEvent(callEvent, taskId, kind, conversationId);
        break;
      case TIMEOUT:
        handleAgentTimedoutCallEvent(callEvent, taskId, kind, conversationId);
        break;
      case MACHINE:
        break;
      case COMPLETED:
        handleAgentCompletedCallEvent(callEvent, taskId, kind, conversationId);
        break;
      default:
        break;
    }
  }
  
  private void handleCustomerAnsweredCallEvent(CallEvent callEvent, String taskId) {
    if (null != taskId ) {
      
    }
  }

  private void handleCustomerCompletedCallEvent(CallEvent callEvent, String taskId) {
    if (null != taskId ) {
      
    }
  }

  private void handleAgentAnsweredCallEvent(CallEvent callEvent, String taskId, String kind,
      String conversationId) {
    if (null != taskId ) {
      String number = null;
      // get task context
      TaskDto task = getTask(taskId);
      if (null != task) {
        AttributeGroupDto context = task.getUserContext();
        if (null != context) {
          AttributeValueDto numberDto = context.get("xnumber");
          if (null == numberDto) {
            numberDto = context.get("from");
          }
          
          if (null != numberDto) {
            number = getStringFromAttributeValueDto(numberDto);
          }
        }
      }

      // call customer
      if (null != number) {
        if (false == makeCall(normilizeNumber(number), taskId, conversationId)) {
          LOGGER.error("Failed connect customer: {}, task: {}, conv: {}", 
              number, taskId, conversationId);
        }
      }
    }
  }
  
  private void handleAgentCompletedCallEvent(CallEvent callEvent, String taskId, String kind,
      String conversationId) {

    if (null != taskId ) {
      updateTaskServiceState(taskId, TaskState.completed);
      //TODO: Hangup other leg
    }
  }
  
  private void handleAgentTimedoutCallEvent(CallEvent callEvent, String taskId, String kind,
      String conversationId) {
    LOGGER.trace("handleAgentTimedoutCallEvent");

    if (null != taskId ) {
      // TODO: decide how to report this state. may be it is better to report it as timed out?
      updateTaskServiceState(taskId, TaskState.completed);
    }
  }

  private void updateTaskServiceState(String taskId, TaskState state) {
    UpdateTaskArg updTaskReq = new UpdateTaskArg();
    updTaskReq.setState(state);

    try {
      LOGGER.trace("Update task: {} in router as completed", taskId);
      taskServiceClient.update(updTaskReq,
          new RouterObjectId(taskId, configuration.getCommsRouterId()));
    } catch (BadValueException | NotFoundException e) {
      LOGGER.error("Failed to update task state with error: {}", e.getLocalizedMessage());
      e.printStackTrace();
    } catch (Exception ex) {
      LOGGER.error("Failed to update task state with error: {}", ex.getLocalizedMessage());
      ex.printStackTrace();
    }
  }

  private TaskDto getTask(String taskId) {
    TaskDto result = null;
    
    try {
      LOGGER.trace("Get task: {}", taskId);
      result = taskServiceClient.get(new RouterObjectId(taskId, configuration.getCommsRouterId()));
    } catch (NotFoundException e) {
      LOGGER.error("Failed to update task state with error: {}", e.getLocalizedMessage());
      e.printStackTrace();
    } catch (Exception ex) {
      LOGGER.error("Failed to update task state with error: {}", ex.getLocalizedMessage());
      ex.printStackTrace();
    }
    
    return result;
  }

  private void hangupCall(String uuid) {
    try {
      LOGGER.debug("Request to hangup call uuid: {}", uuid);
      nexMoService.getVoiceClient().modifyCall(uuid, "hangup");
    } catch (IOException | NexmoClientException e) {
      LOGGER.error("Hangup call failed with error: {}", e.getLocalizedMessage());
      e.printStackTrace();
    } catch (Exception e) {
      LOGGER.error("Hangup call error: {}", e.getLocalizedMessage());
      e.printStackTrace();
    }
  }
  
  private boolean makeCall(String number, String taskId, String conversationId) {

    boolean flagOk = false;
    CallEvent callEvent = null;
    
    try {
      // obtain agent's endpoint to be called
      Endpoint epTo = NexMoModelFactory.createEndpoint(number);
      Endpoint epFrom =
          NexMoModelFactory.createEndpoint(configuration.getAssociatedPhone().toLog());
      URI uri = UriBuilder.fromPath(configuration.getCallbackBaseUrl()).path("answer_outbound")
          .path(conversationId).queryParam("kind", "callback_agent").build();

      String answerUrl = uri.toString();

      // prepare to start a call to the agent
      Call callRequest = new Call(epTo, epFrom, answerUrl);

      // set event url
      URI evturi = 
          UriBuilder.fromPath(configuration.getCallbackBaseUrl())
          .path("event")
          .queryParam("kind", "callback_agent")
          .queryParam("taskId", taskId)
          .queryParam("callId", conversationId)
          .build();

      String eventUrl = evturi.toString();
      callRequest.setEventUrl(eventUrl);

      // start a call to the agent
      callEvent = nexMoService.getVoiceClient().createCall(callRequest);
      LOGGER.debug("calling at: {} with uuid: {}", number, callEvent.getUuid());
      flagOk = true;
    
    } catch (IOException | NexmoClientException e) {
      // Would not call agent. Mark the task as complete with error.
      LOGGER.error("Failed to make a call to agent with error: {}", e.getLocalizedMessage());
      e.printStackTrace();
    } catch (Exception ex) {
      // Would not call agent. Mark the task as complete with error.
      LOGGER.error("Failed to make a call to agent with error: {}", ex.getLocalizedMessage());
      ex.printStackTrace();
    }

    try {
      if (flagOk) {
        AttributeGroupDto userContext = new AttributeGroupDto();
        UpdateTaskContext updateCtx = new UpdateTaskContext();
        userContext.put("customer_uuid", new StringAttributeValueDto(callEvent.getUuid()));
        updateCtx.setUserContext(userContext);
        taskServiceClient.update(updateCtx, 
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
  
  private String normilizeNumber(String number) {
    String normalized = number;
    if (!number.startsWith("+")) {
      normalized = "+" + number;
    } else {
      normalized = number;
    }
    
    return normalized;
  }
  
  
  private String getStringFromAttributeValueDto(AttributeValueDto valueDto) {

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
}
