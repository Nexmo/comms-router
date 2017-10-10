package com.softavail.commsrouter.nexmoapp.jpa;

import com.softavail.commsrouter.jpa.GenericRepository;
import com.softavail.commsrouter.jpa.JpaTransactionManager;
import com.softavail.commsrouter.nexmoapp.domain.Session;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class SessionRepository extends GenericRepository<Session> {

  public SessionRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

}
