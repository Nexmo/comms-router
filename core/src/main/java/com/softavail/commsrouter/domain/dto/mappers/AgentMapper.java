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

package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.AgentQueueMapping;

/**
 * @author ikrustev
 */
public class AgentMapper extends RouterObjectEntityMapper<AgentDto, Agent> {

  public final AttributesMapper attributesMapper;

  public AgentMapper(AttributesMapper attributesMapper) {
    this.attributesMapper = attributesMapper;
  }

  @Override
  public AgentDto toDto(Agent jpa) {
    AgentDto dto = new AgentDto();
    copyRef(dto, jpa);
    dto.setCapabilities(attributesMapper.toDto(jpa.getCapabilities()));
    dto.setAddress(jpa.getAddress());
    dto.setName(jpa.getName());
    dto.setDescription(jpa.getDescription());
    dto.setState(jpa.getState());
    dto.setQueueRefs(
        createIdList(jpa.getAgentQueueMappings()
            .stream()
            .map(AgentQueueMapping::getQueue)));
    dto.setLastTimeAtBusyState(jpa.getLastTimeAtBusyState());
    return dto;
  }

}
