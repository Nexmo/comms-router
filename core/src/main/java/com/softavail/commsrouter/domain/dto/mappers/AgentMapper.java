/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.domain.Agent;

/**
 *
 * @author ikrustev
 */
public class AgentMapper extends EntityMapper<AgentDto, Agent> {

  public final AttributesMapper attributesMapper;

  public AgentMapper(AttributesMapper attributesMapper) {
    this.attributesMapper = attributesMapper;
  }

  @Override
  public AgentDto toDto(Agent jpa) {
    AgentDto dto = new AgentDto();
    copyId(dto, jpa);
    dto.setCapabilities(attributesMapper.toDto(jpa.getCapabilities()));
    dto.setAddress(jpa.getAddress());
    dto.setState(jpa.getState());
    dto.setQueueIds(createIdList(jpa.getQueues()));
    return dto;
  }

}
