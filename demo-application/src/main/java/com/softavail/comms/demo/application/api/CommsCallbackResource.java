package com.softavail.comms.demo.application.api;

import com.nexmo.client.NexmoClientException;
import com.nexmo.client.voice.Call;
import com.nexmo.client.voice.CallEvent;
import com.nexmo.client.voice.Endpoint;
import com.softavail.comms.demo.application.client.TaskServiceClient;
import com.softavail.comms.demo.application.factory.NexMoModelFactory;
import com.softavail.comms.demo.application.impl.NexMoConversationServiceImpl;
import com.softavail.comms.demo.application.model.NexMoCall;
import com.softavail.comms.demo.application.model.NexMoConversation;
import com.softavail.comms.demo.application.model.NexMoConversationStatus;
import com.softavail.comms.demo.application.model.UpdateNexMoConversationArg;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.ConversationService;
import com.softavail.comms.demo.application.services.NexMoService;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Path("/comms_callback")
public class CommsCallbackResource {

  private static final Logger LOGGER = LogManager.getLogger(CommsCallbackResource.class);

  private ConversationService conversationService = new NexMoConversationServiceImpl();

  @Inject
  Configuration configuration;

  @Inject
  NexMoService nexMoService;

  @Inject
  TaskServiceClient taskServiceClient;

  @POST
  @Path("/{taskId}")
  public void taskAnswer(
      @PathParam("taskId") String taskId,
      @QueryParam("callId") final String conversationId,
      AgentDto agent) {
    LOGGER.debug("/comms_callback/{}", taskId);
    LOGGER.debug("task: {}", agent);

    boolean wouldConnectAgent = true;
    NexMoConversation conversation = null;

    do {
      conversation = conversationService.getConversation(conversationId);

      if (null == conversation) {
        LOGGER.error("cannot find conversation with id {}", conversationId);
        wouldConnectAgent = false;
        break;
      }

      // obtain agent's endpoint to be called
      Endpoint epAgent = NexMoModelFactory.createEndpoint(agent.getAddress());
      Endpoint epFrom =
          NexMoModelFactory.createEndpoint(configuration.getAssociatedPhone().toLog());
      String answerUrl =
          configuration.getCallbackBaseUrl() + "answer_outbound/" + conversation.getId();
      // prepare to start a call to the agent
      Call callRequest = new Call(epAgent, epFrom, answerUrl);
      try {

        // start a call to the agent
        CallEvent callEvent = nexMoService.getVoiceClient().createCall(callRequest);

        NexMoCall callee = new NexMoCall(callEvent.getUuid(), callEvent.getConversationUuid());
        callee.setDirection(callEvent.getDirection());
        callee.setStatus(callEvent.getStatus());

        UpdateNexMoConversationArg updateArg =
            new UpdateNexMoConversationArg(NexMoConversationStatus.CONNECTING);
        updateArg.setAgent(callee);
        conversationService.updateConversation(conversationId, updateArg);
      } catch (IOException | NexmoClientException e) {
        // Would not call agent. Mark the task as complete with error.
        LOGGER.error("Failed to make a call to agent with error: {}", e.getLocalizedMessage());
        wouldConnectAgent = false;
        e.printStackTrace();
      } catch (Exception ex) {
        // Would not call agent. Mark the task as complete with error.
        LOGGER.error("Failed to make a call to agent with error: {}", ex.getLocalizedMessage());
        wouldConnectAgent = false;
        ex.printStackTrace();
      }

    }
    while (false);

    if (!wouldConnectAgent) {

      // reset the state of the conversation
      if (conversation != null) {
        UpdateNexMoConversationArg updateArg =
            new UpdateNexMoConversationArg(NexMoConversationStatus.STARTED);
        conversationService.updateConversation(conversation.getId(), updateArg);
      }

      UpdateTaskArg updateArg = new UpdateTaskArg();
      updateArg.setState(TaskState.completed);

      try {
        taskServiceClient.update(updateArg);
      } catch (BadValueException | NotFoundException e1) {
        LOGGER.error("Failed to report task as completed with eror: {}", e1.getLocalizedMessage());
        e1.printStackTrace();
      } catch (Exception e2) {
        LOGGER.error("Failed to report task as completed with eror: {}", e2.getLocalizedMessage());
        e2.printStackTrace();
      }
    }
  }

}
