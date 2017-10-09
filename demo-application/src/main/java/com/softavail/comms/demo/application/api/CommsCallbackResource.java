package com.softavail.comms.demo.application.api;

import com.nexmo.client.NexmoClientException;
import com.nexmo.client.voice.Call;
import com.nexmo.client.voice.CallEvent;
import com.nexmo.client.voice.Endpoint;
import com.softavail.comms.demo.application.factory.NexMoModelFactory;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.NexMoService;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.client.TaskServiceClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.net.URI;
import java.util.UUID;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;

@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
@Path("/comms_callback")
public class CommsCallbackResource {

  private static final Logger LOGGER = LogManager.getLogger(CommsCallbackResource.class);

  @Inject
  Configuration configuration;

  @Inject
  NexMoService nexMoService;

  @Inject
  TaskServiceClient taskServiceClient;

  
  @POST
  public void taskAssignmentAnswer(
      @QueryParam("callId") final String conversationId,
      final TaskAssignmentDto taskAssignment) {

    // TODO 
  }

  @POST
  @Path("/{taskId}")
  public void taskAnswer(
      @PathParam("taskId") String taskId,
      @QueryParam("kind") final String kind, 
      @QueryParam("callId") final String conversationId, 
      AgentDto agent) {

    LOGGER.debug("/comms_callback/{}", taskId);
    LOGGER.debug("agent: {}", agent);

    if (null != kind && kind.equals("callback")) {
      handleCallbackTask(taskId, agent);
    } else {
      handleRegularTask(taskId, conversationId, agent);
    }
    
  }

  private void handleRegularTask(
      final String taskId, 
      final String conversationId,
      final AgentDto agent) {

    boolean flagOk = false;
    CallEvent callEvent = null;
    
    try {

      // obtain agent's endpoint to be called
      Endpoint epAgent = NexMoModelFactory.createEndpoint(agent.getAddress());
      Endpoint epFrom =
          NexMoModelFactory.createEndpoint(configuration.getAssociatedPhone().toLog());
      URI uri = UriBuilder.fromPath(configuration.getCallbackBaseUrl())
          .path("answer_outbound")
          .path(conversationId)
          .queryParam("kind", "regular")
          .build();
      
      String answerUrl = uri.toString();
      
      // prepare to start a call to the agent
      Call callRequest = new Call(epAgent, epFrom, answerUrl);

      // set event url
      URI evturi = UriBuilder.fromPath(configuration.getCallbackBaseUrl())
          .path("event")
          .queryParam("kind", "regular")
          .queryParam("taskId", taskId)
          .queryParam("callId", conversationId)
          .build();

      String eventUrl = evturi.toString();
      callRequest.setEventUrl(eventUrl);
      
      // start a call to the agent
      callEvent = nexMoService.getVoiceClient().createCall(callRequest);
      LOGGER.debug("calling agent at: {} with uuid: {}", agent.getAddress(), callEvent.getUuid());
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

      if (false == flagOk) {
        UpdateTaskArg updateArg = new UpdateTaskArg();
        // TODO: we need failed state for a task
        updateArg.setState(TaskState.completed);
        RouterObjectId taskObjectId = new RouterObjectId(taskId, configuration.getCommsRouterId());
        taskServiceClient.update(updateArg, taskObjectId);
      } else {
        AttributeGroupDto userContext = new AttributeGroupDto();
        UpdateTaskContext updateCtx = new UpdateTaskContext();
        RouterObjectId taskObjectId = new RouterObjectId(taskId, configuration.getCommsRouterId());
        userContext.put("agent_uuid", new StringAttributeValueDto(callEvent.getUuid()));
        updateCtx.setUserContext(userContext);
        taskServiceClient.update(updateCtx, taskObjectId);
      }
    } catch (BadValueException | NotFoundException e1) {
      LOGGER.error("Failed to report task as completed with eror: {}", e1.getLocalizedMessage());
      e1.printStackTrace();
    } catch (Exception e2) {
      LOGGER.error("Failed to report task as completed with eror: {}", e2.getLocalizedMessage());
      e2.printStackTrace();
    }
  }

  private void handleCallbackTask(
      final String taskId, 
      final AgentDto agent) {

    boolean flagOk = false;
    CallEvent callEvent = null;
    
    try {
      String conversationId = "conv-" + UUID.randomUUID().toString();

      // obtain agent's endpoint to be called
      Endpoint epAgent = NexMoModelFactory.createEndpoint(agent.getAddress());
      Endpoint epFrom =
          NexMoModelFactory.createEndpoint(configuration.getAssociatedPhone().toLog());
      URI uri = UriBuilder.fromPath(configuration.getCallbackBaseUrl()).path("answer_outbound")
          .path(conversationId).queryParam("kind", "callback_agent").build();

      String answerUrl = uri.toString();

      // prepare to start a call to the agent
      Call callRequest = new Call(epAgent, epFrom, answerUrl);

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
      LOGGER.debug("calling agent at: {} with uuid: {}", agent.getAddress(), callEvent.getUuid());
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
      if (false == flagOk) {
        UpdateTaskArg updateArg = new UpdateTaskArg();
        // TODO: we need failed state for a task
        updateArg.setState(TaskState.completed);
        RouterObjectId taskObjectId = new RouterObjectId(taskId, configuration.getCommsRouterId());
        taskServiceClient.update(updateArg, taskObjectId);
      } else {
        AttributeGroupDto userContext = new AttributeGroupDto();
        UpdateTaskContext updateCtx = new UpdateTaskContext();
        RouterObjectId taskObjectId = new RouterObjectId(taskId, configuration.getCommsRouterId());
        userContext.put("agent_uuid", new StringAttributeValueDto(callEvent.getUuid()));
        updateCtx.setUserContext(userContext);
        taskServiceClient.update(updateCtx, taskObjectId);
      }
    } catch (BadValueException | NotFoundException e1) {
      LOGGER.error("Failed to report task as completed with eror: {}", e1.getLocalizedMessage());
      e1.printStackTrace();
    } catch (Exception e2) {
      LOGGER.error("Failed to report task as completed with eror: {}", e2.getLocalizedMessage());
      e2.printStackTrace();
    }
  }

  /*
  @POST
  @Path("/{taskId}")
  public void oldtaskAnswer(
      @PathParam("taskId") String taskId,
      @QueryParam("callId") final String conversationId,
      final TaskAssignmentDto taskAssignment) {

    AgentDto agent = taskAssignment.getAgent();

    LOGGER.debug("/comms_callback/{}", taskId);
    LOGGER.debug("agent: {}", agent);

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
        RouterObjectId taskObjectId = new RouterObjectId(taskId, configuration.getCommsRouterId());
        taskServiceClient.update(updateArg, taskObjectId);
      } catch (BadValueException | NotFoundException e1) {
        LOGGER.error("Failed to report task as completed with eror: {}", e1.getLocalizedMessage());
        e1.printStackTrace();
      } catch (Exception e2) {
        LOGGER.error("Failed to report task as completed with eror: {}", e2.getLocalizedMessage());
        e2.printStackTrace();
      }
    }
  }*/

}
