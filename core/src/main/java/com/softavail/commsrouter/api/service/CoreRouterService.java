/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
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

    Router router = new Router(createArg, objectId);
    em.persist(router);
    RouterDto routerDto = entityMapper.toDto(router);
    return new ApiObjectId(routerDto);
  }

}
