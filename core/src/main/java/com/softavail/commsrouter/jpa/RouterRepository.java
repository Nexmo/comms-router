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
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.domain.Router;
import java.util.List;
import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class RouterRepository extends GenericRepository<Router> {

  public RouterRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

  public Router getByRef(EntityManager em, String ref) throws NotFoundException {
    Router router = getByRefNoThrow(em, ref);
    if (router != null) {
      return router;
    }
    throw new NotFoundException(entityClass.getSimpleName() + " " + ref + " not found");
  }

  @SuppressWarnings("unchecked")
  private Router getByRefNoThrow(EntityManager em, String ref) throws NotFoundException {

    String query = "SELECT r FROM Router r WHERE r.ref = :ref";

    List<Router> result = em.createQuery(query).setParameter("ref", ref).getResultList();

    if (result.isEmpty()) {
      return null;
    }

    assert result.size() == 1;

    return result.get(0);
  }

  public void deleteByRef(EntityManager em, String ref) throws CommsRouterException {
    Router router = getByRefNoThrow(em, ref);
    if (router != null) {
      em.remove(router);
    }
  }

}
