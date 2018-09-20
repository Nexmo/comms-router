package com.softavail.comms.demo.application.services;

import com.softavail.comms.demo.application.model.NexMoCall;
import com.softavail.comms.demo.application.model.NexMoConversation;
import com.softavail.comms.demo.application.model.UpdateNexMoConversationArg;

/** .
 * 
 * @author acho
 *
 */
public interface ConversationService {

  /** Get conversation by id.
   * 
   * @param id id of the conversation
   * @return The conversation if exists or null
   */
  public NexMoConversation getConversation(String id);
  
  /** Creates a new conversation or returns existing one.
   * 
   * @param id id of the conversation
   * @return the conversation object
   */
  public NexMoConversation createConversation(String id, NexMoCall caller, String taskId);

  /**
   * Updates a single call.
   * @param call Call to be updated
   */
  public void updateCall(NexMoCall call);

  /**
   * . 
   * @param uuid removes a call
   */
  public void removeCallWithUuid(String uuid);

  /**
   * .
   * @param uuid
   * @return
   */
  public NexMoCall getCallWithUuid(String uuid);

  /**
   * Retrieve an inbound call by conversation id. 
   * @param conversationId conversationId
   * @return Call object if found otherwise null
   */
  public NexMoCall getInboundCallWithConversationId(String conversationId);

  /**
   * Retrieve an outbound call by conversation id. 
   * @param conversationId conversationId
   * @return Call object if found otherwise null
   */
  public NexMoCall getOutboundCallWithConversationId(String conversationId);

  /**
   * .
   * @param uuid
   * @return
   */
  public NexMoConversation getConversationWithInboundCall(String uuid);  

  /**
   * .
   * @param uuid
   * @return
   */
  public NexMoConversation getConversationWithOutboundCall(String uuid);  
  
  /**
   * .
   * @param taskId
   * @return
   */
  public NexMoConversation getConversationWithTaskId(String taskId);
  
  /**
   * 
   * @param conversationId
   * @param updateArg
   */
  public void updateConversation(String conversationId, UpdateNexMoConversationArg updateArg);
}
