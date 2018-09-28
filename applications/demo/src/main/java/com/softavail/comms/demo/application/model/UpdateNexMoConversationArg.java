package com.softavail.comms.demo.application.model;

public class UpdateNexMoConversationArg {

  private NexMoConversationStatus status;
  
  private NexMoCall agent;
  
  private NexMoCall caller;
  
  private boolean dontUpdateStatus;

  public UpdateNexMoConversationArg() {
    this.setDontUpdateStatus(true); 
  }

  public UpdateNexMoConversationArg(NexMoConversationStatus status) {
    this.setStatus(status);
    this.setDontUpdateStatus(false); 
  }
  
  public NexMoConversationStatus getStatus() {
    return status;
  }

  public void setStatus(NexMoConversationStatus status) {
    this.status = status;
  }

  public NexMoCall getAgent() {
    return agent;
  }

  public void setAgent(NexMoCall agent) {
    this.agent = agent;
  }

  public boolean isDontUpdateStatus() {
    return dontUpdateStatus;
  }

  public void setDontUpdateStatus(boolean dontUpdateStatus) {
    this.dontUpdateStatus = dontUpdateStatus;
  }

  public NexMoCall getCaller() {
    return caller;
  }

  public void setCaller(NexMoCall caller) {
    this.caller = caller;
  }
  
  
}
