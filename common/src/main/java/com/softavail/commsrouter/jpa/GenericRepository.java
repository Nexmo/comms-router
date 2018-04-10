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

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaQuery;

/**
 *
 * @author ikrustev
 */
public class GenericRepository<ENTITYT> {

  protected final Class<ENTITYT> entityClass;

  protected final JpaTransactionManager transactionManager;

  @SuppressWarnings("unchecked")
  public GenericRepository(JpaTransactionManager transaction) {

    Type tp = getClass().getGenericSuperclass();
    ParameterizedType pt = (ParameterizedType) tp;
    this.entityClass = (Class<ENTITYT>) (pt.getActualTypeArguments()[0]);
    this.transactionManager = transaction;
  }

  public Class<ENTITYT> getEntityClass() {
    return entityClass;
  }

  public ENTITYT get(EntityManager em, Long id) throws NotFoundException {

    ENTITYT jpaEntity = em.find(entityClass, id);
    if (jpaEntity == null) {
      throw new NotFoundException(entityClass.getSimpleName() + " " + id + " not found");
    }
    return jpaEntity;
  }

  public List<ENTITYT> list(EntityManager em)
      throws CommsRouterException {

    CriteriaQuery<ENTITYT> cq = em.getCriteriaBuilder().createQuery(entityClass);
    cq.select(cq.from(entityClass));
    List<ENTITYT> result = em.createQuery(cq).getResultList();
    return result;
  }

  public ENTITYT delete(EntityManager em, Long id) throws CommsRouterException {
    ENTITYT entity = em.find(entityClass, id);
    if (entity != null) {
      em.remove(entity);
    }
    return entity;
  }

}
