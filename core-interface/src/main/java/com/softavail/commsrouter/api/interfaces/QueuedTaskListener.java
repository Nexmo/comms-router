/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.interfaces;

import com.softavail.commsrouter.api.dto.model.TaskDto;

/**
 *
 * @author ergyunsyuleyman
 */
public interface QueuedTaskListener {

  void onTaskAddedToQueue(TaskDto task);

  void onTaskAssignedToAgent(TaskDto task);

  void onQueuedTaskTimeout(TaskDto task);

  void onQueuedTaskCompleted(TaskDto task);
}
