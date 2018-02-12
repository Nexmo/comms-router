/* 
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.commsrouter.api.dto.arg;

import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;


/**
 * Created by @author mapuo on 04.09.17.
 */
public class CreateAgentArg {

  private String address;
  private AttributeGroupDto capabilities;
  private String name;
  private String description;

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

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public static class Builder{
    private CreateAgentArg agentArg = new CreateAgentArg();

    public Builder(String name) {
      agentArg.setDescription(name);
      agentArg.setName(name);
    }
    
    public Builder name(String name) {
      agentArg.setName(name);
      return this;
    }
    
    public Builder description(String description) {
      agentArg.setDescription(description);
      return this;
    }

    public Builder address(String address) {
      agentArg.setAddress(address);
      return this;
    }

    public Builder capabilities(AttributeGroupDto capabilities) {
      agentArg.setCapabilities(capabilities);
      return this;
    }
    
    public CreateAgentArg build() {
      return agentArg;
    }

  }
  
}
