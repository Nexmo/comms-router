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
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Path("/event_outbound")
public class NexMoEventOutResource {

  private static final Logger LOGGER = LogManager.getLogger(NexMoEventOutResource.class);

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
      NexmoCallEvent callEvent) {

    if (callEvent != null) {
      LOGGER.debug("/event_outbound with call uuid: {} status: {} direction: {}",
          callEvent.getUuid(), callEvent.getStatus(), callEvent.getDirection());

      if (kind != null && kind.equals("callback_agent")) {
        handleAgentCallEvent(callEvent, taskId);
      } else if (kind != null && kind.equals("callback_customer")) {
        handleCustomerCallEvent(callEvent, taskId);
      } else if (kind != null && kind.equals("regular_agent")) {
        handleAgentCallEvent(callEvent, taskId);
      }
    }

    Response response = Response.ok().build();
    LOGGER.debug("/event_outbound response: {}", response);
    return response;
  }

  private void handleCustomerCallEvent(NexmoCallEvent callEvent, String taskId) {
    LOGGER.trace("handleCustomerCallEvent");

    switch (callEvent.getStatus()) {
      case STARTED:
        break;
      case RINGING:
        break;
      case ANSWERED:
        handleCustomerAnsweredCallEvent(callEvent, taskId);
        break;
      case MACHINE:
        break;
      case COMPLETED:
        handleCustomerCompletedCallEvent(callEvent, taskId);
        break;
      case TIMEOUT:
      case FAILED:
      case REJECTED:
      case CANCELLED:
      case BUSY:
        handleCustomerFailedCallEvent(callEvent, taskId);
        break;
      default:
        break;
    }
  }

  private void handleAgentCallEvent(NexmoCallEvent callEvent, String taskId) {
    LOGGER.trace("handleAgentCallEvent");
    switch (callEvent.getStatus()) {
      case STARTED:
        break;
      case RINGING:
        break;
      case ANSWERED:
        handleAgentAnsweredCallEvent(callEvent, taskId);
        break;
      case MACHINE:
        break;
      case COMPLETED:
        handleAgentCompletedCallEvent(callEvent, taskId);
        break;
      case TIMEOUT:
      case FAILED:
      case REJECTED:
      case CANCELLED:
      case BUSY:
        handleAgentFailedCallEvent(callEvent, taskId);
        break;
      default:
        break;
    }
  }

  private void handleCustomerAnsweredCallEvent(NexmoCallEvent callEvent, String taskId) {
    if (null != taskId ) {

    }
  }

  private void handleCustomerCompletedCallEvent(NexmoCallEvent callEvent, String taskId) {
    if (null != taskId ) {
      TaskDto task = getTask(taskId);

      if (null != task && null != task.getUserContext()) {
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
      } else {
        LOGGER.warn("No task or taskContext, cannot handle customer completed call event, ");
      }
    }
  }

  private void handleCustomerFailedCallEvent(NexmoCallEvent callEvent, String taskId) {
    if (null != taskId ) {
      TaskDto task = getTask(taskId);

      if (null != task && null != task.getUserContext()) {
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
      } else {
        LOGGER.warn("No task or taskContext, cannot handle customer completed call event, ");
      }
    }
  }

  private void handleAgentAnsweredCallEvent(NexmoCallEvent callEvent, String taskId) {
    if (null != taskId ) {

    }
  }

  private void handleAgentCompletedCallEvent(NexmoCallEvent callEvent, String taskId) {

    if (null != taskId ) {

      TaskDto task = getTask(taskId);

      if (null != task) {
        updateTaskServiceState(taskId, TaskState.completed);

        // hangup customer's call if present
        if (null != task.getUserContext()) {
          AttributeValueDto customerUuidDto = task.getUserContext().get("customer_uuid");
          if (null != customerUuidDto) {
            String customerUuid = getStringFromAttributeValueDto(customerUuidDto);
            if (null != customerUuid) {
              hangupCall(customerUuid);
            } else {
              LOGGER.error("Cannot extract string from Dto");
            }
          } else {
            LOGGER.warn("Cannot hangup customer's leg because \"customer_uuid\" is not available");
          }
        }
      } else {
        LOGGER.warn("No task or taskContext, cannot handle agent completed call event, ");
      }
    }
  }

  private void handleAgentFailedCallEvent(NexmoCallEvent callEvent, String taskId) {
    LOGGER.trace("handleAgentTimedoutCallEvent");

    if (null != taskId ) {
      // TODO: decide how to report this state. may be it is better to report it as timed out?
      updateTaskServiceState(taskId, TaskState.completed);

      TaskDto task = getTask(taskId);
      // hang up customer's call if present
      if (null != task && null != task.getUserContext()) {
        AttributeValueDto customerUuidDto = task.getUserContext().get("customer_uuid");
        if (null != customerUuidDto) {
          String customerUuid = getStringFromAttributeValueDto(customerUuidDto);
          if (null != customerUuid) {
            hangupCall(customerUuid);
          } else {
            LOGGER.debug("Cannot extract string from Dto");
          }
        } else {
          LOGGER.debug("Cannot hangup customer's leg because \"customer_uuid\" is not available");
        }
      }
    }
  }

  private void updateTaskServiceState(String taskRef, TaskState state) {
    UpdateTaskArg updTaskReq = new UpdateTaskArg();
    updTaskReq.setState(state);

    try {
      LOGGER.trace("Update task: {} in router as completed", taskRef);
      taskServiceClient.update(updTaskReq,
          new RouterObjectRef(taskRef, configuration.getCommsRouterId()));
    } catch (BadValueException | NotFoundException e) {
      LOGGER.error("Failed to update task state with error: {}", e.getLocalizedMessage());
      e.printStackTrace();
    } catch (Exception ex) {
      LOGGER.error("Failed to update task state with error: {}", ex.getLocalizedMessage());
      ex.printStackTrace();
    }
  }

  private TaskDto getTask(String taskRef) {
    TaskDto result = null;

    try {
      LOGGER.trace("Get task: {}", taskRef);
      result =
          taskServiceClient.get(new RouterObjectRef(taskRef, configuration.getCommsRouterId()));
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
