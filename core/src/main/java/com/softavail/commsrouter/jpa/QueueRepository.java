/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.domain.Queue;

import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class QueueRepository extends RouterObjectRepository<Queue> {

  public QueueRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

  public long getQueueSize(EntityManager em, String queueId)
      throws CommsRouterException {

    String qlString = "SELECT COUNT(t.id) FROM Task t "
        + "JOIN t.queue q WHERE q.id = :queueId AND t.state = :state";

    return (long) em.createQuery(qlString)
        .setParameter("queueId", queueId)
        .setParameter("state", TaskState.waiting)
        .getSingleResult();
  }

}
