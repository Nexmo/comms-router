/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.jpa.result.TaskEnumerableResult;


/**
 * @author ikrustev
 */
public class TaskRepository extends RouterObjectRepository<Task> {

  public TaskRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }
  
  public TaskEnumerableResult enumerableResultFilteredByWaitingState() {

    TaskEnumerableResult result =
        new TaskEnumerableResult(this.transactionManager, TaskState.waiting);
    
    return result;
  }
  
}
