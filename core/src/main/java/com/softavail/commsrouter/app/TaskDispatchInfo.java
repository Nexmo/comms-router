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

package com.softavail.commsrouter.app;

/**
 *
 * @author ikrustev
 */
public class TaskDispatchInfo {

  private Long routerId;
  private Long taskId;
  private String taskRef;
  private Long queueId;
  private long queuePosition;
  private Long queuedTimeout;

  public Long getRouterId() {
    return routerId;
  }

  public void setRouterId(Long routerId) {
    this.routerId = routerId;
  }

  public Long getTaskId() {
    return taskId;
  }

  public void setTaskId(Long taskId) {
    this.taskId = taskId;
  }

  public String getTaskRef() {
    return taskRef;
  }

  public void setTaskRef(String taskRef) {
    this.taskRef = taskRef;
  }

  public Long getQueueId() {
    return queueId;
  }

  public void setQueueId(Long queueId) {
    this.queueId = queueId;
  }

  public long getQueuePosition() {
    return queuePosition;
  }

  public void setQueuePosition(long queuePosition) {
    this.queuePosition = queuePosition;
  }

  public Long getQueuedTimeout() {
    return queuedTimeout;
  }

  public void setQueuedTimeout(Long queuedTimeout) {
    this.queuedTimeout = queuedTimeout;
  }

}
