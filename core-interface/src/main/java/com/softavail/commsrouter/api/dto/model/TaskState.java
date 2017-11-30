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

package com.softavail.commsrouter.api.dto.model;

/**
 *
 * @author ikrustev
 */
public enum TaskState {

  waiting(true), canceled(true), assigned(false), completed(true);

  private boolean deleteAllowed;

  TaskState(boolean deleteAllowed) {
    this.deleteAllowed = deleteAllowed;
  }

  public boolean isWaiting() {
    return this == waiting;
  }

  public boolean isCanceled() {
    return this == canceled;
  }

  public boolean isAssigned() {
    return this == assigned;
  }

  public boolean isCompleted() {
    return this == completed;
  }

  public boolean isDeleteAllowed() {
    return deleteAllowed;
  }

  public boolean isFinal() {
    return this == completed || this == canceled;
  }

}
