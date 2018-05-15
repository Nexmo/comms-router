package com.softavail.comms.demo.application.api;

import com.nexmo.client.NexmoClientException;
import com.nexmo.client.voice.ModifyCallAction;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.NexMoService;
import com.softavail.comms.nexmo.model.NexmoCallEvent;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
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
import java.util.ArrayList;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

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
  public Response callState(NexmoCallEvent callEvent) {

    if (callEvent != null) {
      LOGGER.debug("/event with call uuid: {} status: {} direction: {}",
          callEvent.getUuid(), callEvent.getStatus(), callEvent.getDirection());

      handleCustomerCallEvent(callEvent);
    }

    Response response = Response.ok().build();
    return response;
  }

  private void handleCustomerCallEvent(NexmoCallEvent callEvent) {
    LOGGER.trace("handleCustomerCallEvent");

    switch (callEvent.getStatus()) {
      case STARTED:
        break;
      case RINGING:
        break;
      case ANSWERED:
        handleCustomerAnsweredCallEvent(callEvent);
        break;
      case MACHINE:
        break;
      case COMPLETED:
        handleCustomerCompletedCallEvent(callEvent);
        break;
      case TIMEOUT:
      case FAILED:
      case REJECTED:
      case CANCELLED:
      case BUSY:
        handleCustomerFailedCallEvent(callEvent);
        break;
      default:
        break;
    }
  }

  private void handleCustomerAnsweredCallEvent(NexmoCallEvent callEvent) {
  }

  private void handleCustomerCompletedCallEvent(NexmoCallEvent callEvent) {
    String tag = callEvent.getUuid();
    if (null != tag) {
      TaskDto task = getTaskByTag(tag);

      if (null != task) {
        boolean goodCallbackTask = false;
        AttributeGroupDto userContext = task.getUserContext();
        if (null != userContext) {
          AttributeValueDto kindDto = userContext.get("kind");
          AttributeValueDto stateDto = userContext.get("callback_state");
          
          if (null != kindDto && null != stateDto) {
            String kind = getStringFromAttributeValueDto(kindDto);
            String state = getStringFromAttributeValueDto(stateDto);
            
            if (kind.equalsIgnoreCase("callback") && state.equalsIgnoreCase("completed")) {
              goodCallbackTask = true;
            }
          }
        }
        
        // cancel the task if it is not turned into completed callback
        // and the state is still waiting
        if (!goodCallbackTask && task.getState() == TaskState.waiting) {
          // by canceling a waiting (non-good callback or regular) task we remove it form the 
          // queue so it wont be assigned to an agent
          updateTaskServiceState(task.getRef(), TaskState.canceled);
        }

        // try to hang up agent's call leg if possible
        if (null != task.getUserContext()) {
          AttributeValueDto uuidDto = task.getUserContext().get("agent_uuid");

          if (null != uuidDto) {
            String uuid = getStringFromAttributeValueDto(uuidDto);
            if (null != uuid) {
              hangupCall(uuid);
            } else {
              LOGGER.error("Cannot extract string from Dto");
            }
          } else {
            LOGGER.warn("Cannot hangup agent's leg because \"agent_uuid\" is not available");
          }
        }   
      } else {
        LOGGER.warn("No task or taskContext, cannot handle customer completed call event, ");
      }
    }
  }

  private void handleCustomerFailedCallEvent(NexmoCallEvent callEvent) {
  }

  private void updateTaskServiceState(String taskRef, TaskState state) {
    UpdateTaskArg updTaskReq = new UpdateTaskArg();
    updTaskReq.setState(state);

    try {
      LOGGER.debug("Update task's:{} state:'{}'", taskRef, state);
      taskServiceClient.update(updTaskReq,
          new RouterObjectRef(taskRef, configuration.getCommsRouterId()));
    } catch (BadValueException | NotFoundException e) {
      LOGGER.error("Failed to update task's state with error: {}", e.getLocalizedMessage());
      e.printStackTrace();
    } catch (Exception ex) {
      LOGGER.error("Failed to update task's state with error: {}", ex.getLocalizedMessage());
      ex.printStackTrace();
    }
  }

  private TaskDto getTaskByTag(String tag) {
    TaskDto result = null;

    if (null == tag) {
      return null;
    }
    
    try {
      LOGGER.trace("Get task by tag: {}", tag);
      result = taskServiceClient.getByTag(configuration.getCommsRouterId(), tag);
    } catch (CommsRouterException e) {
      LOGGER.error("Failed to get task by tag with error: {}", e.getLocalizedMessage());
    } catch (Exception ex) {
      LOGGER.error("Failed to get task by tag with error: {}", ex.getLocalizedMessage());
      ex.printStackTrace();
    }

    return result;
  }

  private void hangupCall(String uuid) {
    try {
      LOGGER.debug("Request to hangup call uuid: {}", uuid);
      nexMoService.getVoiceClient().modifyCall(uuid, ModifyCallAction.HANGUP);
    } catch (IOException | NexmoClientException e) {
      LOGGER.error("Hangup call failed with error: {}", e.getLocalizedMessage());
      e.printStackTrace();
    } catch (Exception e) {
      LOGGER.error("Hangup call error: {}", e.getLocalizedMessage());
      e.printStackTrace();
    }
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
        public void handleStringValue(StringAttributeValueDto value) throws CommsRouterException {
          result.add(value.getValue());
        }

        @Override
        public void handleDoubleValue(DoubleAttributeValueDto value) throws CommsRouterException {
          // TODO Auto-generated method stub

        }

        @Override
        public void handleBooleanValue(BooleanAttributeValueDto value) throws CommsRouterException {
          // TODO Auto-generated method stub

        }

        @Override
        public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value)
            throws CommsRouterException {
          // TODO Auto-generated method stub

        }

        @Override
        public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
            throws CommsRouterException {
          // TODO Auto-generated method stub

        }
      });
    } catch (CommsRouterException e) {
      LOGGER.error(e.getLocalizedMessage());
    }

    if (result != null && result.size() > 0) {
      stringValue = result.get(0);
    }

    return stringValue;
  }
}
