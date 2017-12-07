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

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.api.dto.model.AgentState;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.domain.Queue;
import com.softavail.commsrouter.domain.result.MatchResult;

import java.util.List;
import java.util.Optional;
import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class QueueRepository extends RouterObjectRepository<Queue> {

  public QueueRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

  public long getQueueSize(EntityManager em, Long queueId) throws CommsRouterException {

    String qlString = "SELECT COUNT(t.id) FROM Task t "
        + "JOIN t.queue q WHERE q.id = :queueId AND t.state = :state";

    return (long) em.createQuery(qlString)
        .setParameter("queueId", queueId)
        .setParameter("state", TaskState.waiting)
        .getSingleResult();
  }

  public long getQueueSize(EntityManager em, RouterObjectRef queueRef) throws CommsRouterException {

    String qlString = "SELECT COUNT(t.id) FROM Task t " + "JOIN t.queue q JOIN q.router r "
            + "WHERE r.ref = :routerRef AND q.ref = :queueRef AND t.state = :state";

    return (long) em.createQuery(qlString)
        .setParameter("routerRef", queueRef.getRouterRef())
        .setParameter("queueRef", queueRef.getRef())
        .setParameter("state", TaskState.waiting)
        .getSingleResult();
  }

  @SuppressWarnings("unchecked")
  public Optional<MatchResult> findAssignment(EntityManager em, Long queueId)
      throws CommsRouterException {

    String query = "SELECT NEW com.softavail.commsrouter.domain.result.MatchResult(t, a) "
        + "FROM Task t JOIN t.queue q JOIN q.agentQueueMappings m JOIN m.agent a "
        + "WHERE t.state = :taskState AND a.state = :agentState AND q.id = :queueId "
        + "ORDER BY t.priority DESC, t.id ASC, a.lastTimeAtBusyState ASC";

    List<MatchResult> result = em.createQuery(query)
        .setParameter("taskState", TaskState.waiting)
        .setParameter("agentState", AgentState.ready)
        .setParameter("queueId", queueId)
        .setMaxResults(1)
        .getResultList();

    return result.stream().findFirst();
  }

  @SuppressWarnings("unchecked")
  public Optional<MatchResult> findAssignmentForAgent(EntityManager em, Long agentId)
      throws CommsRouterException {

    String query = "SELECT NEW com.softavail.commsrouter.domain.result.MatchResult(t, a) "
        + "FROM Task t JOIN t.queue q JOIN q.agentQueueMappings m JOIN m.agent a "
        + "WHERE t.state = :taskState AND a.state = :agentState AND a.id = :agentId "
        + "ORDER BY t.priority DESC, t.id ASC, a.lastTimeAtBusyState ASC";

    List<MatchResult> result = em.createQuery(query)
        .setParameter("taskState", TaskState.waiting)
        .setParameter("agentState", AgentState.ready)
        .setParameter("agentId", agentId)
        .setMaxResults(1)
        .getResultList();

    return result.stream().findFirst();
  }

}
