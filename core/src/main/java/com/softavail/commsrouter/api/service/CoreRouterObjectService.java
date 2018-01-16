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
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.domain.RouterObject;
import com.softavail.commsrouter.domain.dto.mappers.EntityMapper;
import com.softavail.commsrouter.jpa.RouterObjectRepository;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;
import javax.persistence.EntityManager;
import javax.validation.ValidationException;

/**
 * @author ikrustev
 */
public class CoreRouterObjectService<DTOT extends RouterObjectRef, ENTITYT extends RouterObject>
    implements RouterObjectService<DTOT> {

  protected final Class<DTOT> dtoEntityClass;
  protected final Class<ENTITYT> entityClass;
  protected final AppContext app;
  protected final RouterObjectRepository<ENTITYT> repository;
  protected final EntityMapper<DTOT, ENTITYT> entityMapper;

  @SuppressWarnings("unchecked")
  public CoreRouterObjectService(AppContext app, RouterObjectRepository<ENTITYT> repository,
      EntityMapper<DTOT, ENTITYT> entityMapper) {
    this.app = app;
    this.repository = repository;
    this.entityMapper = entityMapper;

    Type tp = getClass().getGenericSuperclass();
    ParameterizedType pt = (ParameterizedType) tp;
    this.dtoEntityClass = (Class<DTOT>) (pt.getActualTypeArguments()[0]);
    this.entityClass = (Class<ENTITYT>) (pt.getActualTypeArguments()[1]);
  }

  public Class<DTOT> getDtoEntityClass() {
    return dtoEntityClass;
  }

  @Override
  public DTOT get(RouterObjectRef routerObjectRef) throws CommsRouterException {
    return app.db.transactionManager.execute((em) -> {
      ENTITYT entity = repository.get(em, routerObjectRef);
      return entityMapper.toDto(entity);
    });
  }

  @Override
  public List<DTOT> list(String routerRef) throws CommsRouterException {
    return app.db.transactionManager.execute((em) -> {
      List<ENTITYT> list = repository.list(em, routerRef);
      return entityMapper.toDto(list);
    });
  }

  @Override
  @SuppressWarnings("unchecked")
  public PaginatedList<DTOT> list(String routerRef, int page, int perPage)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {

      String simpleName = entityClass.getSimpleName();

      String countString = "SELECT COUNT(e.id) FROM " + simpleName + " e "
          + "JOIN e.router r WHERE r.ref = :routerRef ORDER BY r.id";
      long totalCount = (long) em.createQuery(countString)
          .setParameter("routerRef", routerRef)
          .getSingleResult();

      int startPosition = (page * perPage) - perPage;

      if (totalCount > 0 && totalCount <= startPosition) {
        throw new ValidationException("{resource.list.max.page.number}");
      }

      String qlString =
          "SELECT e FROM " + simpleName + " e JOIN e.router r "
              + "WHERE r.ref = :routerRef ORDER BY r.id";
      List<ENTITYT> jpaResult = em.createQuery(qlString)
          .setParameter("routerRef", routerRef)
          .setFirstResult(startPosition)
          .setMaxResults(perPage)
          .getResultList();

      return new PaginatedList<>(entityMapper.toDto(jpaResult), page, perPage, totalCount);
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
