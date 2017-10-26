package com.softavail.comms.nexmo.ivr;

import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.UriBuilder;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeType;
import com.nexmo.client.voice.ncco.InputNcco;
import com.nexmo.client.voice.ncco.Ncco;
import com.nexmo.client.voice.ncco.TalkNcco;
import com.nexmo.client.voice.servlet.NccoResponse;
import com.nexmo.client.voice.servlet.NccoResponseBuilder;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.nexmo.answer.AnswerStrategyException;
import com.softavail.comms.nexmo.answer.AnswerStrategyWithCallback;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;

public class IvrStrategyWithSimpleFlow implements IvrStrategy {

  private static final Logger LOGGER = LogManager.getLogger(IvrStrategyWithSimpleFlow.class);

  private Configuration configuration;
  
  private HashMap<String, String> requirements;

  private static final String KEY_STATE = "state";
  private static final String KEY_LANGUAGE = "language";
  private static final String KEY_DEPARTMENT = "department";
  
  private static final String STATE_PROMPT_LANGUAGE = "lan";
  private static final String STATE_PROMPT_DEPARTMENT = "dep";
  
  private static final String VOICE_SPANISH = "Penelope";
  private static final String VOICE_ENGLISH = "Kimberly";
  
  private static final String MSG_WELCOME =
      "Hello and thank you for calling comms-router demo service";
  private static final String MSG_PROMPT_LANGUAGE = 
      "Si necesita servicio en español, presione 1";
  
  private static final String MSG_PROMPT_DEPARTMENT_EN =
      "For Sales, press 1. For Technical Support, press 2.";
  private static final String MSG_PROMPT_DEPARTMENT_ES =
      "Para Ventas, presione 1. Para soporte técnico, presione 2.";

  private static final String LANGUAGE_ENGLISH = "en";
  private static final String LANGUAGE_SPANISH = "es";
  private static final String DEPARTMENT_SALES = "sales";
  private static final String DEPARTMENT_SUPPORT = "support";

  @Inject
  AnswerStrategyWithCallback nextStrategy;
  
  @Inject
  IvrStrategyWithSimpleFlow(Configuration configuration) {
    this.configuration = configuration;
  }
  
  @Override
  public String answerInboundCall(String convUuid, String from, String to) {
    LOGGER.debug("answerInboundCall");
    
    if (null == convUuid) {
      return respondWithErrorTalkNcco();
    }
    
    if (null == from) {
      return respondWithErrorTalkNcco();
    }

    if (null == to) {
      return respondWithErrorTalkNcco();
    }

    this.requirements = new HashMap<String, String>();
    this.requirements.put("conversation_uuid", convUuid);
    this.requirements.put("from", from);
    this.requirements.put("to", to);

    // prompt language
    return respondByTransitionToPromptLanguage();
  }

  @Override
  public String continueAnswerInboundCall(JsonNode userInfo, 
      MultivaluedMap<String, String> parameters) {

    LOGGER.debug("continueAnswerInboundCall");

    // Check if the task has been assigned during callback IVR
    if (parameters == null) {
      LOGGER.error("Invalid argument <parameters> is null");
      return respondWithErrorTalkNcco();
    }

    setParameters(parameters);
    
    String state = parameters.getFirst(KEY_STATE);
    if (state == null) {
      LOGGER.error("Missing key<{}> in argument <parameters>", KEY_STATE);
      return respondWithErrorTalkNcco();
    }
    
    String nccoResponse = null;
    
    switch (state) {
      case STATE_PROMPT_LANGUAGE:
        nccoResponse = handlePromptLanguageResponse(userInfo);
        break;
      case STATE_PROMPT_DEPARTMENT:
        nccoResponse = handlePromptDepartmentResponse(userInfo);
        break;
      default:
        nccoResponse = respondWithErrorTalkNcco();
        break;
    }

    return nccoResponse;
  }

  @Override
  public HashMap<String, String> getParamaters() {
    return null;
  }

  private String handlePromptLanguageResponse(JsonNode userInfo) {
    LOGGER.debug("handlePromptLanguageResponse");
    String number = parseDtmfFromUserInfo(userInfo);
    if (number != null && number.equals("1")) {
      // Spanish
      this.requirements.put(KEY_LANGUAGE, LANGUAGE_SPANISH);
    } else {
      // English
      this.requirements.put(KEY_LANGUAGE, LANGUAGE_ENGLISH);
    }

    return respondByTransitionToPromptDepartment(); 
  }
  
  private String handlePromptDepartmentResponse(JsonNode userInfo) {
    LOGGER.debug("handlePromptDepartmentResponse");
    String number = parseDtmfFromUserInfo(userInfo);
    if (number != null && number.equals("1")) {
      // Sales
      LOGGER.debug("will set {}:{}", KEY_DEPARTMENT, DEPARTMENT_SALES);
      this.requirements.put(KEY_DEPARTMENT, DEPARTMENT_SALES);
    } else {
      // Support
      LOGGER.debug("will set {}:{}", KEY_DEPARTMENT, DEPARTMENT_SUPPORT);
      this.requirements.put(KEY_DEPARTMENT, DEPARTMENT_SUPPORT);
    }

    return respondByTransitionToEnd(); 
  }

  private String respondByTransitionToPromptLanguage() {
    UriBuilder uriBuilder = UriBuilder.fromPath(getEventUrl())
        .queryParam(KEY_STATE, STATE_PROMPT_LANGUAGE);
    
    this.requirements.keySet().forEach(key -> {
      uriBuilder.queryParam(key, this.requirements.get(key));
    });

    URI uri = uriBuilder.build();
    String eventUrl = uri.toString();
    InputNcco input  = new InputNcco();
    input.setEventUrl(eventUrl);
    input.setMaxDigits(1);
    input.setTimeOut(5);
    
    TalkNcco welcome = new TalkNcco(MSG_WELCOME);
    welcome.setVoiceName(VOICE_ENGLISH);

    TalkNcco language = new TalkNcco(MSG_PROMPT_LANGUAGE);
    language.setVoiceName(VOICE_SPANISH);
    language.setBargeIn(true);
    
    // preparing a response    
    NccoResponseBuilder builder = new NccoResponseBuilder();
    builder.appendNcco(welcome);
    builder.appendNcco(language);
    builder.appendNcco(input);
    
    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }
  
  private String respondByTransitionToPromptDepartment() {

    UriBuilder uriBuilder = UriBuilder.fromPath(getEventUrl())
        .queryParam(KEY_STATE, STATE_PROMPT_DEPARTMENT);

    this.requirements.keySet().forEach(key -> {
      uriBuilder.queryParam(key, this.requirements.get(key));
    });
    
    URI uri = uriBuilder.build();
    String eventUrl = uri.toString();

    String message = null;
    String voice = null;

    if (keyParameterHasValue(KEY_LANGUAGE, LANGUAGE_SPANISH)) {
      message = MSG_PROMPT_DEPARTMENT_ES;
      voice = VOICE_SPANISH;
    } else {
      message = MSG_PROMPT_DEPARTMENT_EN;
      voice = VOICE_ENGLISH;
    }
    
    TalkNcco department = new TalkNcco(message);
    department.setVoiceName(voice);
    department.setBargeIn(true);
    
    InputNcco input  = new InputNcco();
    input.setEventUrl(eventUrl);
    input.setMaxDigits(1);
    input.setTimeOut(5);
    
    // preparing a response    
    NccoResponseBuilder builder = new NccoResponseBuilder();
    builder.appendNcco(department);
    builder.appendNcco(input);
    
    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }

  private String respondByTransitionToEnd() {

    String answer = null;
    
    try {
      answer = nextStrategy.answerInboundCallWithParams(this.requirements);
    } catch (AnswerStrategyException e) {
      LOGGER.error("answerInboundCallWithParams rised: {}", e.getMessage());
    } catch (Exception ex) {
      LOGGER.error("answerInboundCallWithParams rised Exception: {}", ex.getMessage());
    }
    
    if (answer == null) { 
      answer = respondWithErrorTalkNcco();
    }
    
    return answer;
  }
  
  private String respondWithErrorTalkNcco() {
    // preparing a response
    NccoResponseBuilder builder = new NccoResponseBuilder();
    Ncco talkNcco = new TalkNcco("Sorry we can't serve your request");
    builder.appendNcco(talkNcco);

    // respond
    NccoResponse nccoResponse = builder.getValue();
    return nccoResponse.toJson();
  }

  private String parseDtmfFromUserInfo(JsonNode userInfo) {

    if (userInfo.get("dtmf").getNodeType() == JsonNodeType.STRING) {
      String dtmf = userInfo.get("dtmf").asText();
      int indexOfPound = dtmf.indexOf("#"); 
      if ( indexOfPound != -1) {
        dtmf = dtmf.substring(0, indexOfPound);
      }
      
      return dtmf;
    }
    
    return null;
  }

  private String getEventUrl() {
    return configuration.getNexmoCallbackBaseUrl() + "event_ivr";
  }

  private void setParameters(MultivaluedMap<String, String> parameters) {
    this.requirements = new HashMap<String, String>();
    
    parameters.keySet().forEach(key -> {
      if (false == key.equals(KEY_STATE)) {
        String value = parameters.getFirst(key);
        this.requirements.put(key, value);
      }
    });
  }

  private boolean keyParameterHasValue(String key, String value) {
    boolean flagHas = false;
    
    if (key != null && value != null && this.requirements != null) {
      flagHas = this.requirements.get(key).equals(value);
    }
    
    return flagHas;
  }
  
}
