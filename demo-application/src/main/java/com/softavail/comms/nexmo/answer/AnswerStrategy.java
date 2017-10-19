package com.softavail.comms.nexmo.answer;

import com.fasterxml.jackson.databind.JsonNode;

public interface AnswerStrategy {

  public String answerInboundCall(String convUuid, String from, String to)
      throws AnswerStrategyException;
  
  public String continueAnswerInboundCall(JsonNode userInfo, String taskId, String state)
      throws AnswerStrategyException;
  
  public String answerOutboundCall(String kind, String taskId) 
      throws AnswerStrategyException;

  public String continueAnswerOutboundCall(JsonNode userInfo, String taskId, String action)
      throws AnswerStrategyException;
}
