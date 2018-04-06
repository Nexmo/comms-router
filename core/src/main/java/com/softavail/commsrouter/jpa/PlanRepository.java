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

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.domain.Plan;

import java.util.Date;
import java.util.List;

import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class PlanRepository extends RouterObjectRepository<Plan> {

  public PlanRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

  public void purge(Long planId) throws CommsRouterException {
    transactionManager.executeVoid((EntityManager em) -> {
      super.delete(em, planId);
    });
  }

  @SuppressWarnings("unchecked")
  public List<Long> getDeleted(Date before, int limit) throws CommsRouterException {

    final String query = "SELECT p.id "
        + "FROM Plan p "
        + "WHERE p.deletedTime < :before "
        + "ORDER BY p.deletedTime ASC";

    return transactionManager.execute((EntityManager em) -> {
      List<Long> result = em.createQuery(query)
          .setParameter("before", before)
          .setMaxResults(limit)
          .getResultList();
      return result;
    });
  }

}
