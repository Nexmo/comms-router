package com.softavail.comms.demo.application.api;

import com.nexmo.client.NexmoClientException;
import com.nexmo.client.voice.Call;
import com.nexmo.client.voice.CallEvent;
import com.nexmo.client.voice.Endpoint;
import com.softavail.comms.demo.application.factory.NexMoModelFactory;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.NexMoService;
import com.softavail.comms.nexmo.util.PhoneConverter;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
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
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
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
  public Response taskAssignmentAnswer(
      final TaskAssignmentDto taskAssignment) {

    LOGGER.debug("/comms_callback");
    if (null != taskAssignment) {

      new Thread(new Runnable() {
        @Override
        public void run() {
          doTaskAssignmentAnswer(taskAssignment);
        }
      }).start();
    }

    Response response = Response.ok().build();
    LOGGER.debug("/comms_callback response: {}", response);
    return response;
  }

  private void handleRegularTask(TaskDto task, AgentDto agent) {

    boolean flagOk = false;
    CallEvent callEvent = null;

    try {

      String toNumber = agent.getAddress();//PhoneConverter.normalize(agent.getAddress());
      String fromNumber = PhoneConverter.normalize(configuration.getAssociatedPhone().toLog());
      Endpoint epAgent = NexMoModelFactory.createEndpoint(toNumber);
      Endpoint epFrom = NexMoModelFactory.createEndpoint(fromNumber);

      URI uri = UriBuilder.fromPath(configuration.getNexmoCallbackBaseUrl())
          .path("answer_outbound")
          .queryParam("kind", "regular_agent")
          .queryParam("taskId", task.getRef())
          .build();

      String answerUrl = uri.toString();

      // prepare to start a call to the agent
      Call callRequest = new Call(epAgent, epFrom, answerUrl);

      // set event url
      URI evturi = UriBuilder.fromPath(configuration.getNexmoCallbackBaseUrl())
          .path("event_outbound")
          .queryParam("kind", "regular_agent")
          .queryParam("taskId", task.getRef())
          .build();

      String eventUrl = evturi.toString();
      callRequest.setEventUrl(eventUrl);

      // start a call to the agent
      LOGGER.debug("calling agent at: {}", epAgent.toLog());
      callEvent = nexMoService.getVoiceClient().createCall(callRequest);
      if (callEvent != null && callEvent.getUuid() != null) {
        LOGGER.debug("uuid: {}", callEvent.getUuid());
        flagOk = true;
      }

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
        RouterObjectRef taskObjectRef =
            new RouterObjectRef(task.getRef(), configuration.getCommsRouterId());
        taskServiceClient.update(updateArg, taskObjectRef);
      } else {
        AttributeGroupDto userContext = new AttributeGroupDto();
        UpdateTaskContext updateCtx = new UpdateTaskContext();
        RouterObjectRef taskObjectRef =
            new RouterObjectRef(task.getRef(), configuration.getCommsRouterId());
        userContext.put("agent_uuid", new StringAttributeValueDto(callEvent.getUuid()));
        updateCtx.setUserContext(userContext);
        taskServiceClient.updateContext(updateCtx, taskObjectRef);
      }
    } catch (BadValueException | NotFoundException e1) {
      LOGGER.error("Failed to report task as completed with eror: {}", e1.getLocalizedMessage());
      e1.printStackTrace();
    } catch (Exception e2) {
      LOGGER.error("Failed to report task as completed with eror: {}", e2.getLocalizedMessage());
      e2.printStackTrace();
    }
  }

  private void handleCallbackTask(TaskDto task, AgentDto agent) {

    boolean flagOk = false;
    CallEvent callEvent = null;

    try {
      // obtain agent's endpoint to be called
      String toNumber = agent.getAddress();//PhoneConverter.normalize(agent.getAddress());
      String fromNumber = PhoneConverter.normalize(configuration.getAssociatedPhone().toLog());
      Endpoint epAgent = NexMoModelFactory.createEndpoint(toNumber);
      Endpoint epFrom = NexMoModelFactory.createEndpoint(fromNumber);

      NexMoModelFactory.createEndpoint(configuration.getAssociatedPhone().toLog());
      URI uri = UriBuilder.fromPath(configuration.getNexmoCallbackBaseUrl())
          .path("answer_outbound")
          .queryParam("kind", "callback_agent")
          .queryParam("taskId", task.getRef())
          .build();

      String answerUrl = uri.toString();

      // prepare to start a call to the agent
      Call callRequest = new Call(epAgent, epFrom, answerUrl);

      // set event url
      URI evturi =
          UriBuilder.fromPath(configuration.getNexmoCallbackBaseUrl())
          .path("event_outbound")
          .queryParam("kind", "callback_agent")
          .queryParam("taskId", task.getRef())
          .build();

      String eventUrl = evturi.toString();
      callRequest.setEventUrl(eventUrl);

      // start a call to the agent
      LOGGER.debug("calling agent at: {}, answ_url: {}, evt_url: {}",
          epAgent.toLog(), answerUrl, eventUrl);

      callEvent = nexMoService.getVoiceClient().createCall(callRequest);
      if (callEvent != null && callEvent.getUuid() != null) {
        LOGGER.debug("uuid: {}", callEvent.getUuid());
        flagOk = true;
      }

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
        RouterObjectRef taskObjectRef =
            new RouterObjectRef(task.getRef(), configuration.getCommsRouterId());
        taskServiceClient.update(updateArg, taskObjectRef);
      } else {
        AttributeGroupDto userContext = new AttributeGroupDto();
        UpdateTaskContext updateCtx = new UpdateTaskContext();
        RouterObjectRef taskObjectRef =
            new RouterObjectRef(task.getRef(), configuration.getCommsRouterId());
        userContext.put("agent_uuid", new StringAttributeValueDto(callEvent.getUuid()));
        updateCtx.setUserContext(userContext);
        taskServiceClient.updateContext(updateCtx, taskObjectRef);
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
   * @POST
   *
   * @Path("/{taskId}") public void oldtaskAnswer(
   *
   * @PathParam("taskId") String taskId,
   *
   * @QueryParam("callId") final String conversationId, final TaskAssignmentDto taskAssignment) {
   *
   * final AgentDto agent = taskAssignment.getAgent();
   *
   * LOGGER.debug("/comms_callback/{}", taskId); LOGGER.debug("agent: {}", agent);
   *
   * boolean wouldConnectAgent = true; NexMoConversation conversation = null;
   *
   * do { conversation = conversationService.getConversation(conversationId);
   *
   * if (null == conversation) { LOGGER.error("cannot find conversation with id {}",
   * conversationId); wouldConnectAgent = false; break; }
   *
   * // obtain agent's endpoint to be called Endpoint epAgent =
   * NexMoModelFactory.createEndpoint(agent.getAddress()); Endpoint epFrom =
   * NexMoModelFactory.createEndpoint(configuration.getAssociatedPhone().toLog()); String answerUrl
   * = configuration.getNexmoCallbackBaseUrl() + "answer_outbound/" + conversation.getId(); //
   * prepare to start a call to the agent Call callRequest = new Call(epAgent, epFrom, answerUrl);
   * try {
   *
   * // start a call to the agent CallEvent callEvent =
   * nexMoService.getVoiceClient().createCall(callRequest); if (callEvent != null &&
   * callEvent.getUuid() != null && callEvent.getConversationUuid() != null && callEvent.getStatus()
   * != null) { LOGGER.debug("calling agent with uuid:{}, conv_uuid: {}, status:{}",
   * callEvent.getUuid(), callEvent.getConversationUuid(), callEvent.getStatus());
   *
   * NexMoCall callee = new NexMoCall(callEvent.getUuid(), callEvent.getConversationUuid());
   * callee.setDirection(NexMoCallDirection.OUTBOUND); callee.setStatus(NexMoCallStatus.STARTED);
   *
   * UpdateNexMoConversationArg updateArg = new
   * UpdateNexMoConversationArg(NexMoConversationStatus.CONNECTING); updateArg.setAgent(callee);
   * conversationService.updateConversation(conversationId, updateArg); } else { LOGGER.warn(
   * "Could not call agent. NexMo voice client returned invalid callEvent"); wouldConnectAgent =
   * false; } } catch (IOException | NexmoClientException e) { // Would not call agent. Mark the
   * task as complete with error. LOGGER.error("Failed to make a call to agent with error: {}",
   * e.getLocalizedMessage()); wouldConnectAgent = false; e.printStackTrace(); } catch (Exception
   * ex) { // Would not call agent. Mark the task as complete with error. LOGGER.error(
   * "Failed to make a call to agent with error: {}", ex.getLocalizedMessage()); wouldConnectAgent =
   * false; ex.printStackTrace(); }
   *
   * } while (false);
   *
   * if (!wouldConnectAgent) {
   *
   * // reset the state of the conversation if (conversation != null) { UpdateNexMoConversationArg
   * updateArg = new UpdateNexMoConversationArg(NexMoConversationStatus.STARTED);
   * conversationService.updateConversation(conversation.getId(), updateArg); }
   *
   * UpdateTaskArg updateArg = new UpdateTaskArg(); updateArg.setState(TaskState.completed);
   *
   * try { RouterObjectRef taskObjectId = new RouterObjectRef(taskId,
   * configuration.getCommsRouterId()); taskServiceClient.update(updateArg, taskObjectId); } catch
   * (BadValueException | NotFoundException e1) { LOGGER.error(
   * "Failed to report task as completed with eror: {}", e1.getLocalizedMessage());
   * e1.printStackTrace(); } catch (Exception e2) { LOGGER.error(
   * "Failed to report task as completed with eror: {}", e2.getLocalizedMessage());
   * e2.printStackTrace(); } } }
   */


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

  private void doTaskAssignmentAnswer(
      final TaskAssignmentDto taskAssignment) {

    LOGGER.debug("agent: {}", taskAssignment.getAgent());
    LOGGER.debug("task: {}", taskAssignment.getTask());

    try {
      final TaskDto task = taskAssignment.getTask();
      if (null != task) {
        LOGGER.trace("Able to get task from taskAssignment");
        boolean isCallback = false;
        AttributeGroupDto context = task.getUserContext();

        if (null != context) {
          LOGGER.trace("Able to get task's userContext");
          String kind = attributeGroupDtogetString("kind", context);
          LOGGER.trace("Task's kind:{}", kind);
          if (null != kind && kind.equals("callback")) {
            LOGGER.trace("mark task as callback");
            isCallback = true;
          }
        }

        if (isCallback) {
          LOGGER.debug("About to handle callback task:{}", task.getRef());
          handleCallbackTask(task, taskAssignment.getAgent());
        } else {
          LOGGER.debug("About to handle regular task:{}", task.getRef());
          handleRegularTask(task, taskAssignment.getAgent());
        }
      } else {
        LOGGER.warn("Could not get task from taskAssignment");
      }
    } catch (Exception e) {
      LOGGER.error("Error in taskAssignmentAnswer: {}", e.getMessage());
    }
  }

}
