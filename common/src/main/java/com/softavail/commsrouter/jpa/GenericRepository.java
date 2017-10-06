/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
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

  public ENTITYT get(EntityManager em, String id) throws NotFoundException {
    ENTITYT jpaEntity = em.find(entityClass, id);
    if (jpaEntity == null) {
      throw new NotFoundException(entityClass.getSimpleName() + ": " + id + " not found");
    }
    return jpaEntity;
  }

  public List<ENTITYT> list(EntityManager em) throws CommsRouterException {
    CriteriaQuery<ENTITYT> cq = em.getCriteriaBuilder().createQuery(entityClass);
    cq.select(cq.from(entityClass));
    List<ENTITYT> result = em.createQuery(cq).getResultList();
    return result;
  }

  public void delete(EntityManager em, String id)
      throws CommsRouterException {

    ENTITYT entity = em.find(entityClass, id);
    if (entity != null) {
      em.remove(entity);
    }
  }

}
