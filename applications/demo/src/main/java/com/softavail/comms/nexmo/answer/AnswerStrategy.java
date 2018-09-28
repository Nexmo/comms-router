package com.softavail.comms.nexmo.answer;

import com.fasterxml.jackson.databind.JsonNode;

import java.util.Map;

public interface AnswerStrategy {

  public String answerInboundCallWithParams(Map<String, String> requirements, 
      Map<String, String> userContext) 
      throws AnswerStrategyException;
  
  public String answerInboundCall(String convUuid, String from, String to, String tag)
      throws AnswerStrategyException;
  
  public String continueAnswerInboundCall(JsonNode userInfo, String taskId, String state)
      throws AnswerStrategyException;
  
  public String answerOutboundCall(String kind, String taskId) 
      throws AnswerStrategyException;

  public String continueAnswerOutboundCall(JsonNode userInfo, String taskId, String action)
      throws AnswerStrategyException;
}
