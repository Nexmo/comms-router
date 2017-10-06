package com.softavail.commsrouter.api.dto.model;

/**
 * Created by @author mapuo on 05.10.17.
 */
public class CreatedTaskDto extends TaskDto {

  private final Long queueTasks;

  public CreatedTaskDto(TaskDto taskDto, Long queueTasks) {
    super(taskDto);
    this.queueTasks = queueTasks;
  }

  public Long getQueueTasks() {
    return queueTasks;
  }

}
