package com.softavail.comms.nexmo.answer;

import javax.ws.rs.core.MultivaluedMap;

import com.fasterxml.jackson.databind.JsonNode;

public interface AnswerStrategy {

  public String answerInboundCall(String convUuid, String from, String to)
      throws AnswerStrategyException;
  
  public String continueAnswerInboundCall(JsonNode userInfo, String taskId, String state)
      throws AnswerStrategyException;
}
