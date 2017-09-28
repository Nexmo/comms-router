/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.arg;

import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.model.TaskState;

/**
 *
 * @author ikrustev
 */
public class UpdateTaskArg {
  private TaskState state;

  public TaskState getState() {
    return state;
  }

  public void setState(TaskState state) {
    this.state = state;
  }
}
