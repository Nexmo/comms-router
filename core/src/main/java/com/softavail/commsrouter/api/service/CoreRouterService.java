/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.RouterService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.util.Fields;

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
      Router router = new Router(ensureIdPresent(createArg));
      em.persist(router);
      return entityMapper.toDto(router);
    });
  }

  @Override
  public void update(UpdateRouterArg updateArg)
      throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Router router = app.db.router.get(em, updateArg.getId());
      Fields.update(router::setName, router.getName(), updateArg.getName());
      Fields.update(router::setDescription, router.getDescription(), updateArg.getDescription());
    });
  }

}
