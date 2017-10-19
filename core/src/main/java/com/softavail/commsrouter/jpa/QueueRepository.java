/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.domain.result.MatchResult;

import java.util.List;
import java.util.Optional;
import javax.persistence.EntityManager;
import javax.persistence.LockModeType;

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

  @SuppressWarnings("unchecked")
  public Optional<MatchResult> findAssignment(EntityManager em, String queueId)
      throws CommsRouterException {

    String routerId = get(em, queueId).getRouterId();
    em.find(Router.class, routerId, LockModeType.PESSIMISTIC_WRITE);

    String query = "SELECT NEW com.softavail.commsrouter.domain.result.MatchResult(t, ag) "
        + "FROM Task t JOIN t.queue q JOIN q.agents a JOIN Agent ag ON ag.id = a.id "
        + "WHERE t.state = :taskState AND a.state = :agentState AND q.id = :queueId "
        + "ORDER BY t.priority DESC";

    List<MatchResult> result = em.createQuery(query)
        .setParameter("taskState", TaskState.waiting)
        .setParameter("agentState", AgentState.ready)
        .setParameter("queueId", queueId)
        .setMaxResults(1)
        .getResultList();

    return result.stream().findFirst();
  }

}
