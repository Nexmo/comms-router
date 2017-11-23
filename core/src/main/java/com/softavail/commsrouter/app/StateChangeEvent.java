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
 * Created by @author mapuo on 19.10.17.
 */
public class StateChangeEvent {

  private final Long queueId;
  private final QueueProcessorState oldState;
  private final QueueProcessorState newState;

  public StateChangeEvent(Long queueId,
      QueueProcessorState oldState, QueueProcessorState newState) {

    this.queueId = queueId;
    this.oldState = oldState;
    this.newState = newState;
  }

  public Long getQueueId() {
    return queueId;
  }

  public QueueProcessorState getOldState() {
    return oldState;
  }

  public QueueProcessorState getNewState() {
    return newState;
  }

}
