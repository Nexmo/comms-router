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

import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.domain.RouterObject;

import java.util.List;
import java.util.Objects;
import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class RouterObjectRepository<ENTITYT extends RouterObject>
    extends GenericRepository<ENTITYT> {

  public RouterObjectRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

  public ENTITYT get(EntityManager em, RouterObjectRef routerObjectRef) throws NotFoundException {
    ENTITYT entity = get(em, routerObjectRef.getRef());
    if (entity != null
        && Objects.equals(entity.getRouter().getRef(), routerObjectRef.getRouterRef())) {
      return entity;
    }
    throw new NotFoundException(entityClass.getSimpleName() + " " + routerObjectRef + " not found");
  }

  @SuppressWarnings("unchecked")
  public List<ENTITYT> list(EntityManager em, String routerRef) {
    return em
        .createQuery("SELECT e FROM " + entityClass.getSimpleName()
            + " e JOIN e.router r WHERE r.ref = :routerRef")
        .setParameter("routerRef", routerRef).getResultList();
  }

  public void delete(RouterObjectRef routerObjectRef) throws CommsRouterException {
    transactionManager.executeVoid((em) -> {
      ENTITYT entity = em.find(entityClass, routerObjectRef.getRef());
      if (entity != null
          && Objects.equals(entity.getRouter().getRef(), routerObjectRef.getRouterRef())) {
        em.remove(entity);
      }
    });
  }

}
