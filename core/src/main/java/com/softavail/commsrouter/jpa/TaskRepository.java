/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.domain.Task;

/**
 * @author ikrustev
 */
public class TaskRepository extends RouterObjectRepository<Task> {

  public TaskRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

}
