package com.softavail.comms.nexmo.answer;

import java.util.Map;

import javax.ws.rs.core.MultivaluedMap;

import com.fasterxml.jackson.databind.JsonNode;

public interface AnswerStrategy {

  public String answerInboundCall(final String convUuid, final String from, final String to)
      throws AnswerStrategyException;
  
  public String continueAnswerInboundCall(final JsonNode userInfo,
      final MultivaluedMap<String, String> context) throws AnswerStrategyException;
}
