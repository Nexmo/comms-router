package com.softavail.commsrouter.api.interfaces;

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;

/**
 * Created by @author mapuo on 04.09.17.
 */
public interface RouterService extends ApiObjectService<RouterDto> {

  RouterDto create(CreateRouterArg createArg) throws CommsRouterException;

  void update(UpdateRouterArg updateArg, ApiObjectId objectId) throws CommsRouterException;

  RouterDto put(CreateRouterArg createArg, ApiObjectId objectId) throws CommsRouterException;
}
