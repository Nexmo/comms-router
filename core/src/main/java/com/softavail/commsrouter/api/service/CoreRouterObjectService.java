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

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PaginationHelper;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.domain.RouterObject;
import com.softavail.commsrouter.domain.dto.mappers.EntityMapper;
import com.softavail.commsrouter.jpa.RouterObjectRepository;

import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;

/**
 * @author ikrustev
 */
public class CoreRouterObjectService<DTOT extends RouterObjectRef, ENTITYT extends RouterObject>
    extends CoreApiObjectService<DTOT, ENTITYT>
    implements RouterObjectService<DTOT> {

  protected final AppContext app;
  protected final RouterObjectRepository<ENTITYT> repository;

  @SuppressWarnings("unchecked")
  public CoreRouterObjectService(AppContext app, RouterObjectRepository<ENTITYT> repository,
      EntityMapper<DTOT, ENTITYT> entityMapper) {

    super(app.db.transactionManager, entityMapper);

    this.app = app;
    this.repository = repository;
  }

  @Override
  public DTOT get(RouterObjectRef routerObjectRef)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      ENTITYT entity = repository.get(em, routerObjectRef);
      return entityMapper.toDto(entity);
    });
  }

  @Override
  @SuppressWarnings("unchecked")
  public PaginatedList<DTOT> list(PagingRequest request)
      throws CommsRouterException {

    return app.db.transactionManager.execute(em -> {

      Long entityId = PaginationHelper.getEntityId(entityClass, request.getToken());

      CriteriaBuilder cb = em.getCriteriaBuilder();
      CriteriaQuery<ENTITYT> query = cb.createQuery(entityClass);
      Root<ENTITYT> root = query.from(entityClass);
      Join<ENTITYT, Router> router = root.join("router");
      query.select(
          root);
      query.where(
          cb.gt(root.get("id"), entityId),
          cb.equal(router.get("ref"), request.getRouterRef()));
      query.orderBy(
          cb.asc(root.get("id")));

      List<ENTITYT> jpaResult = em.createQuery(query)
          .setMaxResults(request.getPerPage())
          .getResultList();

      String nextToken = getNextToken(jpaResult, request.getPerPage());

      return new PaginatedList<>(entityMapper.toDto(jpaResult), nextToken);
    });
  }

  @Override
  public void delete(RouterObjectRef routerObjectRef) throws CommsRouterException {
    app.db.transactionManager.executeVoid((em) -> {
      repository.delete(em, routerObjectRef);
    });
  }

  protected Router getRouter(EntityManager em, RouterObjectRef routerObjectRef)
      throws NotFoundException {

    return app.db.router.getByRef(em, routerObjectRef.getRouterRef());
  }

}
