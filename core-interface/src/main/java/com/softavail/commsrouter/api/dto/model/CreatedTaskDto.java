package com.softavail.commsrouter.api.dto.model;

import com.fasterxml.jackson.annotation.JsonIgnore;

/**
 * Created by @author mapuo on 05.10.17.
 */
public class CreatedTaskDto extends ApiObjectId {

  @JsonIgnore
  private String queueId;

  private Long queueTasks;

  public CreatedTaskDto() {}

  public CreatedTaskDto(ApiObjectId taskDto, String queueId, Long queueTasks) {
    super(taskDto);
    this.queueId = queueId;
    this.queueTasks = queueTasks;
  }

  public String getQueueId() {
    return queueId;
  }

  public Long getQueueTasks() {
    return queueTasks;
  }

}
