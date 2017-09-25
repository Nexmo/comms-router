/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.domain.Queue;

/**
 * @author ikrustev
 */
public class QueueRepository extends RouterObjectRepository<Queue> {

  public QueueRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

}
