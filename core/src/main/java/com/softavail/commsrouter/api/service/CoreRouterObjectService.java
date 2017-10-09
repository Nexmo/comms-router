/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.RouterObject;
import com.softavail.commsrouter.domain.dto.mappers.EntityMapper;
import com.softavail.commsrouter.jpa.RouterObjectRepository;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;
import javax.validation.ValidationException;

/**
 * @author ikrustev
 */
public class CoreRouterObjectService<DTOENTITYT extends RouterObjectId, ENTITYT extends RouterObject>
    extends CoreService
    implements RouterObjectService<DTOENTITYT> {

  protected final Class<DTOENTITYT> dtoEntityClass;
  protected final Class<ENTITYT> entityClass;
  protected final AppContext app;
  protected final RouterObjectRepository<ENTITYT> repo;
  protected final EntityMapper<DTOENTITYT, ENTITYT> entityMapper;

  @SuppressWarnings("unchecked")
  public CoreRouterObjectService(AppContext app, RouterObjectRepository<ENTITYT> repo,
      EntityMapper<DTOENTITYT, ENTITYT> entityMapper) {
    this.app = app;
    this.repo = repo;
    this.entityMapper = entityMapper;

    Type tp = getClass().getGenericSuperclass();
    ParameterizedType pt = (ParameterizedType) tp;
    this.dtoEntityClass = (Class<DTOENTITYT>) (pt.getActualTypeArguments()[0]);
    this.entityClass = (Class<ENTITYT>) (pt.getActualTypeArguments()[1]);
  }

  public Class<DTOENTITYT> getDtoEntityClass() {
    return dtoEntityClass;
  }

  @Override
  public DTOENTITYT get(RouterObjectId routerObjectId) throws CommsRouterException {
    return app.db.transactionManager.execute((em) -> {
      ENTITYT entity = repo.get(em, routerObjectId);
      return entityMapper.toDto(entity);
    });
  }

  @Override
  public List<DTOENTITYT> list(String routerId) throws CommsRouterException {
    return app.db.transactionManager.execute((em) -> {
      List<ENTITYT> list = repo.list(em, routerId);
      return entityMapper.toDto(list);
    });
  }

  @Override
  @SuppressWarnings("unchecked")
  public PaginatedList<DTOENTITYT> listPage(String routerId, int page, int perPage)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {

      String simpleName = entityClass.getSimpleName();

      String countString = "SELECT COUNT(e.id) FROM " + simpleName + " e "
          + "WHERE e.routerId = :routerId";
      long totalCount =
          (long) em.createQuery(countString)
              .setParameter("routerId", routerId)
              .getSingleResult();

      int startPosition = (page * perPage) - perPage;

      if (totalCount > 0 && totalCount <= startPosition) {
        throw new ValidationException("{resource.list.max.page.number}");
      }

      String qlString = "SELECT e FROM " + simpleName + " e WHERE e.routerId = :routerId";
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
    repo.delete(routerObjectId);
  }

}
