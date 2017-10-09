package com.softavail.commsrouter.api.dto.model;

/**
 * Created by @author mapuo on 05.10.17.
 */
public class CreatedTaskDto extends ApiObjectId {

  private Long queueTasks;

  public CreatedTaskDto() {}

  public CreatedTaskDto(ApiObjectId taskDto, Long queueTasks) {
    super(taskDto);
    this.queueTasks = queueTasks;
  }

  public Long getQueueTasks() {
    return queueTasks;
  }

}
