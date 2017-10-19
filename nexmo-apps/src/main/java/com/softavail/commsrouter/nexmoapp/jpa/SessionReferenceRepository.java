package com.softavail.commsrouter.nexmoapp.jpa;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.jpa.JpaTransactionManager;
import com.softavail.commsrouter.nexmoapp.domain.SessionReference;
import com.softavail.commsrouter.nexmoapp.domain.SessionReferenceKey;
import com.softavail.commsrouter.nexmoapp.domain.SessionReferenceKey.Type;

import javax.persistence.EntityManager;

/**
 * Created by @author mapuo on 12.10.17.
 */
public class SessionReferenceRepository {

  private JpaTransactionManager transactionManager;

  public SessionReferenceRepository(JpaTransactionManager transactionManager) {
    this.transactionManager = transactionManager;
  }

  public SessionReference get(EntityManager em, Type type, String value)
      throws CommsRouterException {

    final SessionReferenceKey key = new SessionReferenceKey(type, value);
    return get(em, key);
  }

  public SessionReference get(EntityManager em, SessionReferenceKey key)
      throws CommsRouterException {

    return em.find(SessionReference.class, key);
  }

}
