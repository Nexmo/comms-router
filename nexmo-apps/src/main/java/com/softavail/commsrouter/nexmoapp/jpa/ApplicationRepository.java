package com.softavail.commsrouter.nexmoapp.jpa;

import com.softavail.commsrouter.jpa.GenericRepository;
import com.softavail.commsrouter.jpa.JpaTransactionManager;
import com.softavail.commsrouter.nexmoapp.domain.Application;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ApplicationRepository extends GenericRepository<Application> {

  public ApplicationRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

}
