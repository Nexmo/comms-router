package com.softavail.commsrouter.webservice.model;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;

/**
 * Created by @author mapuo on 06.10.17.
 */
public class CreatedTaskFacade extends ApiObjectId {

  private final Long queueTasks;

  public CreatedTaskFacade(CreatedTaskDto taskDto) {
    super(taskDto);
    this.queueTasks = taskDto.getQueueTasks();
  }

  public CreatedTaskFacade(TaskDto taskDto) {
    super(taskDto);
    if (taskDto instanceof CreatedTaskDto) {
      this.queueTasks = ((CreatedTaskDto) taskDto).getQueueTasks();
    } else {
      this.queueTasks = null;
    }
  }

  public Long getQueueTasks() {
    return queueTasks;
  }

}
