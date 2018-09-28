package com.softavail.comms.demo.application.model;



/**
 * 
 * @author acho
 *
 */
public class NexMoConversation {

  private String id;
  private String taskId;
  private NexMoCall caller;
  private NexMoCall agent;
  private NexMoConversationStatus status;
  
  /** .
   * Constructor
   */
  public NexMoConversation(String id, NexMoCall caller, String taskId) {
    
    this.id = id;
    this.caller = caller;
    this.status = NexMoConversationStatus.STARTED;
    this.taskId = taskId;
  }

  public String getId() {
    return id;
  }

  public NexMoCall getCaller() {
    return caller;
  }

  public NexMoCall getAgent() {
    return agent;
  }

  public void setAgent(NexMoCall agent) {
    this.agent = agent;
  }

  public String getTaskId() {
    return taskId;
  }
  
  public NexMoConversationStatus getStatus() {
    return status;
  }

  public void setStatus(NexMoConversationStatus status) {
    this.status = status;
  }
  
  public void updateCaller(NexMoCall caller) {
    this.caller = caller;
  }
  
  @Override
  public NexMoConversation clone() {
    NexMoConversation newObj =
        new NexMoConversation(this.getId(), this.getCaller(), this.getTaskId());

    newObj.setAgent(this.getAgent());
    newObj.setStatus(this.getStatus());
    
    return newObj;
  }
  
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder()
        .append("NexMoConversation [")
        .append("id = ").append(id)
        .append(", status = ").append(status.toString())
        .append(", caller = ").append(caller.getUuid());
    
    if (null != taskId) {
      sb.append(", taskId = ").append(taskId);
    }
    
    if (null != agent) {
      sb.append(", agent = ").append(agent.getUuid());
    }
    
    sb.append("]");
    
    return sb.toString();
  }
}
