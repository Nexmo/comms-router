/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;

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

  public ENTITYT get(EntityManager em, RouterObjectId routerObjectId) throws NotFoundException {
    ENTITYT entity = get(em, routerObjectId.getId());
    if (entity != null && Objects.equals(entity.getRouterId(), routerObjectId.getRouterId())) {
      return entity;
    }
    throw new NotFoundException(entityClass.getSimpleName() + ": " + routerObjectId + " not found");
  }

  @SuppressWarnings("unchecked")
  public List<ENTITYT> list(EntityManager em, String routerId) {
    return em
        .createQuery(
            "SELECT e FROM " + entityClass.getSimpleName() + " e WHERE e.routerId = :routerId")
        .setParameter("routerId", routerId).getResultList();
  }

  public void delete(RouterObjectId routerObjectId) throws CommsRouterException {
    transactionManager.executeVoid((em) -> {
      ENTITYT entity = em.find(entityClass, routerObjectId.getId());
      if (entity != null && Objects.equals(entity.getRouterId(), routerObjectId.getRouterId())) {
        em.remove(entity);
      }
    });
  }

}
