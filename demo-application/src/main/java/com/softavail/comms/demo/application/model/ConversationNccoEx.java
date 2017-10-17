package com.softavail.comms.demo.application.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.nexmo.client.voice.ncco.Ncco;
import com.nexmo.client.voice.ncco.NccoSerializer;

@JsonInclude(value = JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)

public class ConversationNccoEx implements Ncco {

  private static final String ACTION = "conversation";

  private String name;
  private String[] musicOnHoldUrl = null;
  private Boolean startOnEnter = null;
  private Boolean endOnExit = null;
  private Boolean record = null;
  private String[] eventUrl = null;
  private String eventMethod = null;


  public ConversationNccoEx(@JsonProperty("name") String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String[] getMusicOnHoldUrl() {
    return musicOnHoldUrl;
  }

  public void setMusicOnHoldUrl(String musicOnHoldUrl) {
    setMusicOnHoldUrl(new String[]{musicOnHoldUrl});
  }

  @JsonProperty("musicOnHoldUrl")
  public void setMusicOnHoldUrl(String[] musicOnHoldUrl) {
    this.musicOnHoldUrl = musicOnHoldUrl;
  }

  public Boolean getStartOnEnter() {
    return startOnEnter;
  }

  public void setStartOnEnter(Boolean startOnEnter) {
    this.startOnEnter = startOnEnter;
  }

  public Boolean getEndOnExit() {
    return endOnExit;
  }

  public void setEndOnExit(Boolean endOnExit) {
    this.endOnExit = endOnExit;
  }

  public Boolean getRecord() {
    return record;
  }

  public void setRecord(Boolean record) {
    this.record = record;
  }

  public String[] getEventUrl() {
    return eventUrl;
  }

  public void setEventUrl(String eventUrl) {
    setEventUrl(new String[]{eventUrl});
  }

  @JsonProperty("eventUrl")
  public void setEventUrl(String[] eventUrl) {
    this.eventUrl = eventUrl;
  }

  public String getEventMethod() {
    return eventMethod;
  }

  public void setEventMethod(String eventMethod) {
    this.eventMethod = eventMethod;
  }

  @Override
  public String getAction() {
    return ACTION;
  }

  @Override
  public String toJson() {
    return NccoSerializer.getInstance().serializeNcco(this);
  }

}
