/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.ApiObjectService;
import com.softavail.commsrouter.domain.dto.mappers.EntityMapper;
import com.softavail.commsrouter.jpa.GenericRepository;
import com.softavail.commsrouter.jpa.JpaTransactionManager;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;

/**
 * @author ikrustev
 */
public class CoreApiObjectService<DTOENTITYT, ENTITYT>
    implements ApiObjectService<DTOENTITYT> {

  protected final Class<DTOENTITYT> dtoEntityClass;
  protected final JpaTransactionManager transactionManager;
  protected final GenericRepository<ENTITYT> repository;
  protected final EntityMapper<DTOENTITYT, ENTITYT> entityMapper;

  @SuppressWarnings("unchecked")
  public CoreApiObjectService(JpaTransactionManager transactionManager,
      GenericRepository<ENTITYT> repository, EntityMapper<DTOENTITYT, ENTITYT> entityMapper) {
    this.transactionManager = transactionManager;
    this.repository = repository;
    this.entityMapper = entityMapper;

    Type tp = getClass().getGenericSuperclass();
    ParameterizedType pt = (ParameterizedType) tp;
    this.dtoEntityClass = (Class<DTOENTITYT>) (pt.getActualTypeArguments()[0]);
  }

  public Class<DTOENTITYT> getDtoEntityClass() {
    return dtoEntityClass;
  }

  @Override
  public DTOENTITYT get(String id) throws CommsRouterException {
    return transactionManager.execute((em) -> {
      ENTITYT entity = repository.get(em, id);
      return entityMapper.toDto(entity);
    });
  }

  @Override
  public List<DTOENTITYT> list() throws CommsRouterException {
    return transactionManager.execute((em) -> {
      List<ENTITYT> list = repository.list(em);
      return entityMapper.toDto(list);
    });
  }

  @Override
  public void delete(String id) throws CommsRouterException {
    transactionManager.executeVoid((em) -> {
      repository.delete(em, id);
    });
  }

}
