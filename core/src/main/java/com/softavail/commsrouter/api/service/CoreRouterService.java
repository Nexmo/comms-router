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

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.RouterService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;

import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class CoreRouterService extends CoreApiObjectService<RouterDto, Router>
    implements RouterService {

  public CoreRouterService(AppContext app) {
    super(app.db.transactionManager, app.db.router, app.entityMapper.router);
  }

  @Override
  public ApiObjectId create(CreateRouterArg createArg)
      throws CommsRouterException {

    return transactionManager.execute((em) -> {
      ApiObjectId objectId = new ApiObjectId(Uuid.get());
      return doCreate(em, createArg, objectId);
    });
  }

  @Override
  public ApiObjectId create(CreateRouterArg createArg, String routerId)
      throws CommsRouterException {

    return transactionManager.execute((em) -> {
      repository.delete(em, routerId);
      return doCreate(em, createArg, new ApiObjectId(routerId));
    });
  }

  @Override
  public void update(UpdateRouterArg updateArg, String routerId)
      throws CommsRouterException {

    transactionManager.executeVoid((em) -> {
      Router router = repository.get(em, routerId);
      Fields.update(router::setName, router.getName(), updateArg.getName());
      Fields.update(router::setDescription, router.getDescription(), updateArg.getDescription());
    });
  }

  private ApiObjectId doCreate(EntityManager em, CreateRouterArg createArg, ApiObjectId objectId)
      throws CommsRouterException {

    Router router = new Router(objectId);
    if (createArg != null) {
      router.setName(createArg.getName());
      router.setDescription(createArg.getDescription());
    }
    em.persist(router);
    RouterDto routerDto = entityMapper.toDto(router);
    return new ApiObjectId(routerDto);
  }

}
