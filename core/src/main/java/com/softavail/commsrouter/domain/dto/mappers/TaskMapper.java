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

import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.app.TaskDispatchInfo;
import com.softavail.commsrouter.domain.Task;

/**
 *
 * @author ikrustev
 */
public class TaskMapper extends RouterObjectEntityMapper<TaskDto, Task> {

  public final AttributesMapper attributesMapper;

  public TaskMapper(AttributesMapper attributesMapper) {
    this.attributesMapper = attributesMapper;
  }

  public TaskDto toDto(Task jpa) {
    TaskDto dto = new TaskDto();
    copyRef(dto, jpa);
    dto.setCallbackUrl(jpa.getCallbackUrl());
    dto.setRequirements(attributesMapper.toDto(jpa.getRequirements()));
    dto.setUserContext(attributesMapper.toDto(jpa.getUserContext()));
    dto.setState(jpa.getState());
    dto.setPriority(jpa.getPriority());
    dto.setQueueRef(getOptionalId(jpa.getQueue()));
    dto.setAgentRef(getOptionalId(jpa.getAgent()));
    dto.setCreateDate(jpa.getCreateDate());
    dto.setUpdateDate(jpa.getUpdateDate());
    dto.setQueuedTimeout(jpa.getQueuedTimeout());
    dto.setTag(jpa.getTag());
    return dto;
  }

  public TaskDispatchInfo toDispatchInfo(Task task) {
    TaskDispatchInfo result = new TaskDispatchInfo();
    result.setRouterId(task.getRouter().getId());
    result.setTaskId(task.getId());
    result.setTaskRef(task.getRef());
    result.setQueueId(task.getQueue().getId());
    result.setQueuedTimeout(task.getQueuedTimeout());
    return result;
  }

}
