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

import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.PreconditionFailedException;
import com.softavail.commsrouter.domain.ApiObject;
import com.softavail.commsrouter.domain.dto.mappers.EntityMapper;
import com.softavail.commsrouter.jpa.JpaTransactionManager;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * @author ikrustev
 */
public class CoreApiObjectService<DTOENTITYT, ENTITYT extends ApiObject> {

  protected final Class<DTOENTITYT> dtoEntityClass;
  protected final Class<ENTITYT> entityClass;
  protected final JpaTransactionManager transactionManager;
  protected final EntityMapper<DTOENTITYT, ENTITYT> entityMapper;

  @SuppressWarnings("unchecked")
  public CoreApiObjectService(
      JpaTransactionManager transactionManager, EntityMapper<DTOENTITYT, ENTITYT> entityMapper) {

    this.transactionManager = transactionManager;
    this.entityMapper = entityMapper;

    Type tp = getClass().getGenericSuperclass();
    ParameterizedType pt = (ParameterizedType) tp;
    this.dtoEntityClass = (Class<DTOENTITYT>) (pt.getActualTypeArguments()[0]);
    this.entityClass = (Class<ENTITYT>) (pt.getActualTypeArguments()[1]);
  }

  public Class<DTOENTITYT> getDtoEntityClass() {
    return dtoEntityClass;
  }

  protected void checkResourceVersion(ENTITYT entity, ApiObjectRef objectRef)
      throws CommsRouterException {

    if (!entity.hashString().equals(objectRef.getHash())) {
      throw new PreconditionFailedException(entity.hashString(), objectRef.getHash());
    }
  }

}
