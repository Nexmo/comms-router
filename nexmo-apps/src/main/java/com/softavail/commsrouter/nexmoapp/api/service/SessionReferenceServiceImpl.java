package com.softavail.commsrouter.nexmoapp.api.service;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.jpa.JpaTransactionManager;
import com.softavail.commsrouter.nexmoapp.domain.Session;
import com.softavail.commsrouter.nexmoapp.domain.SessionReference;
import com.softavail.commsrouter.nexmoapp.domain.SessionReferenceKey;
import com.softavail.commsrouter.nexmoapp.interfaces.SessionReferenceService;
import com.softavail.commsrouter.nexmoapp.jpa.SessionReferenceRepository;

/**
 * Created by @author mapuo on 12.10.17.
 */
public class SessionReferenceServiceImpl implements SessionReferenceService {

  private final JpaTransactionManager transactionManager;
  private final SessionReferenceRepository repository;

  public SessionReferenceServiceImpl(JpaTransactionManager transactionManager,
      SessionReferenceRepository repository) {
    this.transactionManager = transactionManager;
    this.repository = repository;
  }

  @Override
  public void create(SessionReference reference)
      throws CommsRouterException {

    transactionManager.executeVoid(em ->
        em.persist(reference));
  }

  @Override
  public Session getSessionByKey(SessionReferenceKey key)
      throws CommsRouterException {

    return transactionManager.execute(em ->
        repository.get(em, key).getSession());
  }

}
