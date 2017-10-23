package com.softavail.comms.nexmo.ivr;

import java.util.Map;

import javax.ws.rs.core.MultivaluedMap;

import com.fasterxml.jackson.databind.JsonNode;

public interface IvrStrategy {
  
  public String answerInboundCall(String convUuid, String from, String to);
  
  public String continueAnswerInboundCall(JsonNode userInfo,
      MultivaluedMap<String, String> parameters);
 
  public Map<String, String> getParamaters();
}
