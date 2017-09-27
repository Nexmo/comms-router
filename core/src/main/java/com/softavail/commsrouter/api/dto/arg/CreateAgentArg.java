package com.softavail.commsrouter.api.dto.arg;

import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;

import java.util.List;

/**
 * Created by @author mapuo on 04.09.17.
 */
public class CreateAgentArg {

  private String address;
  private AttributeGroupDto capabilities;
  private AgentState state;

  public String getAddress() {
    return address;
  }

  public void setAddress(String address) {
    this.address = address;
  }

  public AttributeGroupDto getCapabilities() {
    return capabilities;
  }

  public void setCapabilities(AttributeGroupDto capabilities) {
    this.capabilities = capabilities;
  }

  public AgentState getState() {
    return state;
  }

  public void setState(AgentState state) {
    this.state = state;
  }

}
