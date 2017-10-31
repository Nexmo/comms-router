/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
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
public class CoreRouterObjectService<DTOT extends RouterObjectId, ENTITYT extends RouterObject>
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
  public DTOT get(RouterObjectId routerObjectId) throws CommsRouterException {
    return app.db.transactionManager.execute((em) -> {
      ENTITYT entity = repository.get(em, routerObjectId);
      return entityMapper.toDto(entity);
    });
  }

  @Override
  public List<DTOT> list(String routerId) throws CommsRouterException {
    return app.db.transactionManager.execute((em) -> {
      List<ENTITYT> list = repository.list(em, routerId);
      return entityMapper.toDto(list);
    });
  }

  @Override
  @SuppressWarnings("unchecked")
  public PaginatedList<DTOT> list(String routerId, int page, int perPage)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {

      String simpleName = entityClass.getSimpleName();

      String countString = "SELECT COUNT(e.id) FROM " + simpleName + " e "
          + "JOIN e.router r WHERE r.id = :routerId";
      long totalCount =
          (long) em.createQuery(countString)
              .setParameter("routerId", routerId)
              .getSingleResult();

      int startPosition = (page * perPage) - perPage;

      if (totalCount > 0 && totalCount <= startPosition) {
        throw new ValidationException("{resource.list.max.page.number}");
      }

      String qlString = "SELECT e FROM " + simpleName + " e JOIN e.router r WHERE r.id = :routerId";
      List<ENTITYT> jpaResult = em.createQuery(qlString)
          .setParameter("routerId", routerId)
          .setFirstResult(startPosition)
          .setMaxResults(perPage)
          .getResultList();

      return new PaginatedList<>(entityMapper.toDto(jpaResult), page, perPage, totalCount);
    });
  }

  @Override
  public void delete(RouterObjectId routerObjectId) throws CommsRouterException {
    repository.delete(routerObjectId);
  }

  protected Router getRouter(EntityManager em, RouterObjectId routerObjectId)
      throws NotFoundException {

    return app.db.router.get(em, routerObjectId.getRouterId());
  }

}
