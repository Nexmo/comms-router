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
import com.softavail.commsrouter.domain.Agent;
import java.util.List;
import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class AgentRepository extends RouterObjectRepository<Agent> {

  public AgentRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

  public List<Agent> listByState(EntityManager em, String routerRef, AgentState state) {

    String qlString = "select a from Agent a WHERE a.router.ref = :routerRef and a.state = :state";
    List<Agent> list = em.createQuery(qlString).setParameter("routerRef", routerRef)
        .setParameter("state", state).getResultList();
    return list;
  }

}
