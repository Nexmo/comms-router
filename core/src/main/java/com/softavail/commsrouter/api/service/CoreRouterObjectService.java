/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.dto.mappers.EntityMapper;
import com.softavail.commsrouter.jpa.RouterObjectRepository;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;

/**
 * @author ikrustev
 */
public class CoreRouterObjectService<DTOENTITYT extends RouterObject, ENTITYT extends RouterObject>
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
  public DTOENTITYT get(RouterObject routerObjectId) throws CommsRouterException {
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

      String qlString =
          "SELECT e FROM " + entityClass.getSimpleName() + " e "
              + "WHERE e.routerId = :routerId";
      List<ENTITYT> jpaResult = em.createQuery(qlString)
          .setParameter("routerId", routerId)
          .setFirstResult((page - 1) * perPage)
          .setMaxResults(perPage)
          .getResultList();

      String countString = "SELECT COUNT(e.id) FROM " + entityClass.getSimpleName() + " e "
          + "WHERE e.routerId = :routerId";
      long countResult = (long) em.createQuery(countString)
          .setParameter("routerId", routerId)
          .getSingleResult();

      return new PaginatedList<>(entityMapper.toDto(jpaResult), page, perPage, countResult);
    });
  }

  @Override
  public void delete(RouterObject routerObjectId) throws CommsRouterException {
    repo.delete(routerObjectId);
  }

}
