/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.domain.Task;

/**
 *
 * @author ikrustev
 */
public class TaskMapper extends EntityMapper<TaskDto, Task> {

  public final AttributesMapper attributesMapper;

  public TaskMapper(AttributesMapper attributesMapper) {
    this.attributesMapper = attributesMapper;
  }

  public TaskDto toDto(Task jpa) {
    TaskDto dto = new TaskDto();
    copyId(dto, jpa);
    dto.setCallbackUrl(jpa.getCallbackUrl());
    dto.setRequirements(attributesMapper.toDto(jpa.getRequirements()));
    dto.setUserContext(attributesMapper.toDto(jpa.getUserContext()));
    dto.setState(jpa.getState());
    dto.setPlanId(getOptionalId(jpa.getPlan()));
    dto.setQueueId(getOptionalId(jpa.getQueue()));
    dto.setAgentId(getOptionalId(jpa.getAgent()));
    dto.setCreateDate(jpa.getCreateDate());
    dto.setUpdateDate(jpa.getUpdateDate());
    dto.setQueuedTimeout(jpa.getQueuedTimeout());
    return dto;
  }

}
