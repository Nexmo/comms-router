package com.softavail.comms.demo.application.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.nexmo.client.voice.MachineDetection;
import com.nexmo.client.voice.SipEndpoint;
import com.nexmo.client.voice.ncco.Ncco;
import com.nexmo.client.voice.ncco.NccoSerializer;

@JsonInclude(value = JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public class SipConnectNcco implements Ncco {

  private static final String ACTION = "connect";

  private SipEndpoint[] endpoint;
  private String from = null;
  private Integer timeout = null;
  private Integer limit = null;
  private MachineDetection machineDetection = null;
  private String[] eventUrl = null;
  private String eventMethod = null;

  public SipConnectNcco(@JsonProperty("endpoint") SipEndpoint[] endpoint) {
    this.endpoint = endpoint;
  }

  public SipConnectNcco(SipEndpoint endpoint) {
    this(new SipEndpoint[]{endpoint});
  }

  public SipConnectNcco(String uri) {
    this(new SipEndpoint(uri));
  }

  public SipEndpoint[] getEndpoint() {
    return endpoint;
  }

  public void setEndpoint(SipEndpoint endpoint) {
    setEndpoint(new SipEndpoint[]{endpoint});
  }

  @JsonProperty("endpoint")
  public void setEndpoint(SipEndpoint[] endpoint) {
    this.endpoint = endpoint;
  }

  public String getFrom() {
    return from;
  }

  public void setFrom(String from) {
    this.from = from;
  }

  public Integer getTimeout() {
    return timeout;
  }

  public void setTimeout(Integer timeout) {
    this.timeout = timeout;
  }

  public Integer getLimit() {
    return limit;
  }

  public void setLimit(Integer limit) {
    this.limit = limit;
  }

  public MachineDetection getMachineDetection() {
    return machineDetection;
  }

  public void setMachineDetection(MachineDetection machineDetection) {
    this.machineDetection = machineDetection;
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
