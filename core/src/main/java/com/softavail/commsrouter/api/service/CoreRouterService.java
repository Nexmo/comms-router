/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.service;

import javax.persistence.EntityManager;

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

/**
 * @author ikrustev
 */
public class CoreRouterService
    extends CoreApiObjectService<RouterDto, Router>
    implements RouterService {

  public CoreRouterService(AppContext app) {
    super(app, app.db.router, app.entityMapper.router);
  }

  @Override
  public RouterDto create(CreateRouterArg createArg)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      ApiObjectId objectId = new ApiObjectId(Uuid.get());
      return doCreate(em, createArg, objectId);
    });
  }

  @Override
  public void update(UpdateRouterArg updateArg, ApiObjectId objectId)
      throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Router router = app.db.router.get(em, objectId.getId());
      Fields.update(router::setName, router.getName(), updateArg.getName());
      Fields.update(router::setDescription, router.getDescription(), updateArg.getDescription());
    });
  }

  @Override
  public RouterDto put(CreateRouterArg createArg, ApiObjectId objectId)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.router.delete(em, objectId.getId());
      return doCreate(em, createArg, objectId);
    });
  }

  private RouterDto doCreate(EntityManager em, CreateRouterArg createArg, ApiObjectId objectId)
      throws CommsRouterException {

    Router router = new Router(createArg, objectId);
    em.persist(router);
    return entityMapper.toDto(router);
  }
}
