package com.softavail.commsrouter.nexmoapp.api.service;

import com.softavail.commsrouter.api.service.CoreApiObjectService;
import com.softavail.commsrouter.nexmoapp.domain.Session;
import com.softavail.commsrouter.nexmoapp.jpa.TransactionManagerFactory;
import com.softavail.commsrouter.nexmoapp.model.SessionDto;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class SessionService extends CoreApiObjectService<SessionDto, Session> {

  public SessionService() {
    super(TransactionManagerFactory.getTransactionManager(),
        TransactionManagerFactory.getSessionRepository(),
        TransactionManagerFactory.getEntityMappers().sessionMapper);
  }

}
