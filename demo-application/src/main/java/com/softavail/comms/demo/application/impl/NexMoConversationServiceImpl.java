package com.softavail.comms.demo.application.impl;

import com.softavail.comms.demo.application.model.NexMoCall;
import com.softavail.comms.demo.application.model.NexMoCallDirection;
import com.softavail.comms.demo.application.model.NexMoConversation;
import com.softavail.comms.demo.application.model.UpdateNexMoConversationArg;
import com.softavail.comms.demo.application.services.ConversationService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @author acho
 *
 */
public class NexMoConversationServiceImpl implements ConversationService {
  
  private static Map<String, NexMoConversation> conversations = new HashMap<>();
  
  private static Map<String, NexMoCall> calls = new HashMap<>();
  
  private static final Logger LOGGER = LogManager.getLogger(NexMoConversationServiceImpl.class);

  public NexMoConversationServiceImpl() {
    
  }

  @Override
  public NexMoConversation getConversation(String id) {

    NexMoConversation conversation = null;
    
    synchronized (conversations) {
      NexMoConversation target = conversations.get(id);
      if (null != target ) {
        // construct a copy and return it so the caller won't modify the original object
        conversation = target.clone();
      }
    }

    return conversation;
  }

  @Override
  public NexMoConversation createConversation(String id, NexMoCall caller, String taskId) {
    // Get a cloning
    NexMoConversation newObj = getConversation(id);
    
    if (null == newObj) {
      // create it if it does not exist
      NexMoConversation conversation = new NexMoConversation(id, caller, taskId);
      conversations.put(id, conversation);

      // construct a copy and return it so the caller won't modify the original object
      newObj = conversation.clone();
    }
    
    return newObj;
  }

  @Override
  public void updateCall(NexMoCall call) {

    synchronized (calls) {
      NexMoCall target = calls.get(call.getUuid());
      if (null == target) {
        LOGGER.trace("Create call: {}", call);
        calls.put(call.getUuid(), call);
      } else {
        doUpdateCall(target, call);
      }
    }
    
    // Now deal with the conversations

    // Check if the call ends while we have active conversation  
    
  }

  @Override
  public void removeCallWithUuid(String uuid) {
    synchronized (calls) {
      NexMoCall target = calls.get(uuid);
      if (null != target) {
        LOGGER.trace("Delete call: {}", target);
        calls.remove(uuid);
      } 
    }
  }

  private void doUpdateCall(NexMoCall target, NexMoCall call) {
    // update call status
    target.setStatus(call.getStatus());
    LOGGER.trace("Updated call: {}", target);
  }

  @Override
  public NexMoCall getInboundCallWithConversationId(String conversationId) {
    
    LOGGER.trace("Searching inbound call with conv_uuid: {}", conversationId);
    NexMoCall call = null;
    
    synchronized (calls) {
      for (String key: calls.keySet()) {
        NexMoCall incall = calls.get(key);
        if (incall != null && incall.getConversationUuid() != null 
            && incall.getDirection() != null 
            && incall.getConversationUuid().equals(conversationId)
            && incall.getDirection().equals(NexMoCallDirection.INBOUND)) {
          LOGGER.trace("Found outbound call with conv_uuid: {}", conversationId);
          call = incall.clone();
          break;
        }
      }
    }

    return call;
  }
  
  @Override
  public NexMoCall getOutboundCallWithConversationId(String conversationId) {
    LOGGER.trace("Searching outbound call with conv_uuid: {}", conversationId);

    NexMoCall call = null;
    
    synchronized (calls) {
      for (String key: calls.keySet()) {
        NexMoCall outcall = calls.get(key);
        if (outcall != null
            && outcall.getConversationUuid() != null
            && outcall.getDirection() != null
            && outcall.getConversationUuid().equals(conversationId) 
            && outcall.getDirection() == NexMoCallDirection.OUTBOUND) {
          LOGGER.trace("Found outbound call with conv_uuid: {}", conversationId);
          call = outcall.clone();
          break;
        }
      }
    }

    return call;
  }

  @Override
  public NexMoConversation getConversationWithInboundCall(String uuid) {
    LOGGER.trace("Searching conversation with inbound call uuid: {}", uuid);

    NexMoConversation conversation = null;
    
    synchronized (conversations) {
      for (String key: conversations.keySet()) {
        NexMoConversation target = conversations.get(key);
        if (target != null && target.getCaller() != null
            && target.getCaller().getUuid() != null
            && target.getCaller().getUuid().equals(uuid)) {
          LOGGER.trace("Found conversation with inbound call uuid: {}", uuid);
          conversation = target.clone();
          break;
        }
      }
    }

    return conversation;
  }

  @Override
  public NexMoConversation getConversationWithOutboundCall(String uuid) {
    LOGGER.trace("Searching conversation with outbound call uuid: {}", uuid);
    NexMoConversation conversation = null;
    
    synchronized (conversations) {
      for (String key: conversations.keySet()) {
        NexMoConversation target = conversations.get(key);
        if (target != null && target.getAgent() != null
            && target.getAgent().getUuid() != null
            && target.getAgent().getUuid().equals(uuid)) {
          LOGGER.trace("Found conversation with outbound call uuid: {}", uuid);
          conversation = target.clone();
          break;
        }
      }
    }

    return conversation;
  }

  @Override
  public NexMoConversation getConversationWithTaskId(String taskId) {
    LOGGER.trace("Searching conversation with taskid: {}", taskId);
    NexMoConversation conversation = null;
    
    synchronized (conversations) {
      for (String key: conversations.keySet()) {
        NexMoConversation target = conversations.get(key);
        if (target != null && target.getTaskId() != null 
            && target.getTaskId().equals(taskId)) {
          LOGGER.trace("Found conversation with taskid: {}", taskId);
          conversation = target.clone();
          break;
        }
      }
    }

    return conversation;
  }

  @Override
  public void updateConversation(String conversationId, UpdateNexMoConversationArg updateArg) {

    synchronized (conversations) {
      NexMoConversation conversation = conversations.get(conversationId);
      if (null != conversation) {
        if (!updateArg.isDontUpdateStatus()) {
          conversation.setStatus(updateArg.getStatus());
        }
        
        if (updateArg.getAgent() != null) {
          conversation.setAgent(updateArg.getAgent());
        }
        
        if (updateArg.getCaller() != null) {
          conversation.updateCaller(updateArg.getCaller());
        }
        
        LOGGER.trace("Update conversation: {}", conversation);
      }
    }
  }

  @Override
  public NexMoCall getCallWithUuid(String uuid) {
    synchronized (calls) {
      NexMoCall target = calls.get(uuid);
      if (null != target) {
        return target.clone();
      } 
    }
    
    LOGGER.trace("getCallWithUuid not found: {}", uuid);
    return null;
  }
  
}
