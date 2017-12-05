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
import com.softavail.commsrouter.domain.RouterConfig;

import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.LockModeType;

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
  public Router getByRefNoThrow(EntityManager em, String ref) throws NotFoundException {

    String query = "SELECT r FROM Router r WHERE r.ref = :ref";

    List<Router> result = em.createQuery(query).setParameter("ref", ref).getResultList();

    if (result.isEmpty()) {
      return null;
    }

    assert result.size() == 1;

    return result.get(0);
  }

  public Long getIdByRef(EntityManager em, String ref) throws NotFoundException {
    Long id = getIdByRefNoThrow(em, ref);
    if (id != null) {
      return id;
    }
    throw new NotFoundException(entityClass.getSimpleName() + " " + ref + " not found");
  }

  @SuppressWarnings("unchecked")
  public Long getIdByRefNoThrow(EntityManager em, String ref) throws NotFoundException {

    String query = "SELECT r.id FROM Router r WHERE r.ref = :ref";

    List<Long> result = em.createQuery(query).setParameter("ref", ref).getResultList();

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

  public void lock(EntityManager em, Long routerId) {
    em.find(Router.class, routerId, LockModeType.PESSIMISTIC_WRITE);
  }

  public void lockConfig(EntityManager em, Long routerId) {
    em.find(RouterConfig.class, routerId, LockModeType.PESSIMISTIC_WRITE);
  }

  @SuppressWarnings("unchecked")
  public void lockConfigByRef(EntityManager em, String routerRef) throws NotFoundException {
    Long routerId = getIdByRef(em, routerRef);
    lockConfig(em, routerId);
  }

}
