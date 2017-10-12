package com.softavail.comms.demo.application.api;

import com.nexmo.client.NexmoClientException;
import com.nexmo.client.voice.CallDirection;
import com.nexmo.client.voice.CallEvent;
import com.nexmo.client.voice.CallStatus;
import com.softavail.comms.demo.application.impl.NexMoConversationServiceImpl;
import com.softavail.comms.demo.application.model.NexMoCall;
import com.softavail.comms.demo.application.model.NexMoConversation;
import com.softavail.comms.demo.application.model.NexMoConversationStatus;
import com.softavail.comms.demo.application.model.UpdateNexMoConversationArg;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.ConversationService;
import com.softavail.comms.demo.application.services.NexMoService;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.client.TaskServiceClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Path("/event")
public class NexMoEventResource {

  private static final Logger LOGGER = LogManager.getLogger(NexMoEventResource.class);

  private ConversationService conversationService = new NexMoConversationServiceImpl();

  @Inject
  Configuration configuration;

  @Inject
  TaskServiceClient taskServiceClient;

  @Inject
  NexMoService nexMoService;

  @POST
  public Response callState(CallEvent callEvent) {

    if (callEvent != null) {
      LOGGER.debug("/event with call uuid: {} status: {} direction: {}",
          callEvent.getUuid(), callEvent.getStatus(), callEvent.getDirection());

      // Update call info. Creates the call if it does not exist
      NexMoCall call = new NexMoCall(callEvent.getUuid(), callEvent.getConversationUuid());
      call.setDirection(callEvent.getDirection());
      call.setStatus(callEvent.getStatus());

      conversationService.updateCall(call);

      if (callEvent.getDirection() == CallDirection.INBOUND) {
        handleInboundCallEvent(callEvent);
      } else {
        handleOutboundCallEvent(callEvent);
      }

    }

    Response response = Response.ok().build();
    return response;
  }

  @POST
  @Path("/{uuid}")
  public void callState(@PathParam("uuid") String uuid) {
    LOGGER.debug("uuid: {}", uuid);
  }

  private void handleInboundCallEvent(CallEvent callEvent) {
    LOGGER.trace("handleInboundCallEvent");

    NexMoCall call =
        conversationService.getCallWithUuid(callEvent.getUuid());

    if (null != call) {

      switch (call.getStatus()) {
        case STARTED:
          handleStartedInboundCallEvent(callEvent);
          break;
        case RINGING:
          break;
        case ANSWERED:
          handleAnsweredInboundCallEvent(callEvent);
          break;
        case TIMEOUT:
          break;
        case MACHINE:
          break;
        case COMPLETED:
          handleCompletedInboundCallEvent(callEvent);
          break;
        default:
          break;
      }
    } else {
      LOGGER.trace("could not find call with conv_uuid: {}", callEvent.getConversationUuid());
    }
  }

  private void handleOutboundCallEvent(CallEvent callEvent) {
    LOGGER.trace("handleOutboundCallEvent");
    NexMoCall call =
        conversationService.getCallWithUuid(callEvent.getUuid());

    if (null != call) {
      switch (call.getStatus()) {
        case STARTED:
          break;
        case RINGING:
          break;
        case ANSWERED:
          break;
        case TIMEOUT:
          handleTimedoutOutboundCallEvent(callEvent);
          break;
        case MACHINE:
          break;
        case COMPLETED:
          handleCompletedOutboundCallEvent(callEvent);
          break;
        default:
          break;
      }
    } else {
      LOGGER.trace("could not find call with conv_uuid: {}", callEvent.getConversationUuid());
    }
  }

  private void handleStartedInboundCallEvent(CallEvent callEvent) {
    LOGGER.trace("handleStartedInboundCallEvent");

  }

  private void handleAnsweredInboundCallEvent(CallEvent callEvent) {
    LOGGER.trace("handleAnsweredInboundCallEvent");
    // check if we have a call set by its conversation uuid
    // this can happen if answer_inbound callback is called before this event
    NexMoCall call = conversationService.getCallWithUuid(callEvent.getConversationUuid());
    if (null != call) {
      LOGGER.debug("[TEMP_CALL] ****** would remove temp call: {}", call.getUuid());
      conversationService.removeCallWithUuid(callEvent.getConversationUuid());
    }

    NexMoConversation conversation =
        conversationService.getConversationWithInboundCall(callEvent.getConversationUuid());

    if (null != conversation) {
      UpdateNexMoConversationArg updateArg = new UpdateNexMoConversationArg();
      NexMoCall caller = new NexMoCall(callEvent.getUuid(), callEvent.getConversationUuid());
      call.setDirection(callEvent.getDirection());
      call.setStatus(callEvent.getStatus());
      updateArg.setCaller(caller);
      LOGGER.debug("[TEMP_CALL] ****** would update conversation's caller for a temp call: {}",
          callEvent.getConversationUuid());
      conversationService.updateConversation(conversation.getId(), updateArg);
    }

  }

  private void handleCompletedInboundCallEvent(CallEvent callEvent) {
    NexMoConversation conversation =
        conversationService.getConversationWithInboundCall(callEvent.getUuid());

    if (null != conversation) {

      NexMoCall agent = conversation.getAgent();
      if (agent != null) {
        NexMoCall outboundCall = conversationService.getCallWithUuid(agent.getUuid());
        if (null != outboundCall && outboundCall.getStatus() == CallStatus.ANSWERED) {

          try {
            LOGGER.debug("Request to hangup call uuid: {}", outboundCall.getUuid());
            nexMoService.getVoiceClient().modifyCall(outboundCall.getUuid(), "hangup");
          } catch (IOException | NexmoClientException e) {
            LOGGER.error("Hangup call failed with error: {}", e.getLocalizedMessage());
            e.printStackTrace();
          } catch (Exception e) {
            LOGGER.error("Hangup call unexpected error: {}", e.getLocalizedMessage());
            e.printStackTrace();
          }

        }
      }

    } else {
      LOGGER.trace("could not find conversation with inbound call uuid: {}", callEvent.getUuid());
    }

    // Remove the call from the internal storage
    conversationService.removeCallWithUuid(callEvent.getUuid());
  }

  private void handleCompletedOutboundCallEvent(CallEvent callEvent) {
    LOGGER.trace("handleCompletedOutboundCallEvent");
    NexMoConversation conversation =
        conversationService.getConversationWithOutboundCall(callEvent.getUuid());

    if (null != conversation) {
      if (conversation.getStatus() != NexMoConversationStatus.COMPLETED) {
        UpdateNexMoConversationArg updateArg =
            new UpdateNexMoConversationArg(NexMoConversationStatus.COMPLETED);
        conversationService.updateConversation(conversation.getId(), updateArg);

        updateTaskServiceState(conversation.getTaskId(), TaskState.completed);
      }

      NexMoCall caller = conversation.getCaller();
      if (caller != null) {
        NexMoCall inboundCall = conversationService.getCallWithUuid(caller.getUuid());
        if (null != inboundCall && inboundCall.getStatus() == CallStatus.ANSWERED) {

          try {
            LOGGER.debug("Request to hangup call uuid: {}", inboundCall.getUuid());
            nexMoService.getVoiceClient().modifyCall(inboundCall.getUuid(), "hangup");
          } catch (IOException | NexmoClientException e) {
            LOGGER.error("Hangup call failed with error: {}", e.getLocalizedMessage());
            e.printStackTrace();
          } catch (Exception  e) {
            LOGGER.error("Hangup call error: {}", e.getLocalizedMessage());
            e.printStackTrace();
          }
        }
      }
    }

  }

  private void handleTimedoutOutboundCallEvent(CallEvent callEvent) {
    LOGGER.trace("handleTimedoutOutboundCallEvent");
    NexMoConversation conversation =
        conversationService.getConversationWithOutboundCall(callEvent.getUuid());

    if (null != conversation) {
      if (conversation.getStatus() != NexMoConversationStatus.COMPLETED) {
        UpdateNexMoConversationArg updateArg =
            new UpdateNexMoConversationArg(NexMoConversationStatus.STARTED);
        conversationService.updateConversation(conversation.getId(), updateArg);

        // TODO: decide how to report this state. may be it is better to report it as timedout?
        updateTaskServiceState(conversation.getTaskId(), TaskState.completed);
      }
    }

    // Remove the call from the internal storage
    conversationService.removeCallWithUuid(callEvent.getUuid());
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
}
