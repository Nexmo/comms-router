/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model;

/**
 *
 * @author ikrustev
 */
public enum TaskState {

  waiting(true), assigned(false), completed(true);

  private boolean deleteAllowed;

  TaskState(boolean deleteAllowed) {
    this.deleteAllowed = deleteAllowed;
  }

  public boolean isWaiting() {
    return this == waiting;
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

}
