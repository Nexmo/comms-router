package com.softavail.commsrouter.nexmoapp.api.service;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.service.PaginatedApiObjectService;
import com.softavail.commsrouter.domain.AttributeGroup;
import com.softavail.commsrouter.nexmoapp.domain.Module;
import com.softavail.commsrouter.nexmoapp.domain.Session;
import com.softavail.commsrouter.nexmoapp.dto.model.SessionDto;
import com.softavail.commsrouter.nexmoapp.interfaces.SessionService;
import com.softavail.commsrouter.nexmoapp.jpa.TransactionManagerFactory;

import javax.inject.Inject;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class SessionServiceImpl
    extends PaginatedApiObjectService<SessionDto, Session>
    implements SessionService {

  @Inject
  public SessionServiceImpl(TransactionManagerFactory factory) {
    super(
        factory.getTransactionManager(),
        factory.getSessionRepository(),
        factory.getEntityMappers().sessionMapper);
  }

  @Override
  public void create(Session session) throws CommsRouterException {
    transactionManager.executeVoid(em -> em.persist(session));
  }

  @Override
  public void update(String sessionId, AttributeGroup attributeGroup)
      throws CommsRouterException {

    transactionManager.executeVoid(em -> {
      Session session = repository.get(em, sessionId);
      session.setAttributes(attributeGroup);
    });
  }

  @Override
  public void update(String sessionId, Module module)
      throws CommsRouterException {

    transactionManager.executeVoid(em -> {
      Session session = repository.get(em, sessionId);
      session.setCurrentModule(module);
    });
  }

  @Override
  public void delete(String id) throws CommsRouterException {
    throw new CommsRouterException("You cannot delete a Session");
  }

}
