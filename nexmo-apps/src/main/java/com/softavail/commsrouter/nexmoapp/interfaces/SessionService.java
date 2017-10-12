package com.softavail.commsrouter.nexmoapp.interfaces;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.ApiObjectService;
import com.softavail.commsrouter.nexmoapp.domain.Session;
import com.softavail.commsrouter.nexmoapp.dto.model.SessionDto;

/**
 * Created by @author mapuo on 10.10.17.
 */
public interface SessionService extends ApiObjectService<SessionDto> {

  void create(Session session) throws CommsRouterException;

}
