/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.interfaces;

import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.api.exception.CallbackException;

/**
 *
 * @author ikrustev
 */
public interface TaskEventHandler {

  void onTaskAssigned(TaskAssignmentDto taskAssignment)
      throws CallbackException;

}
