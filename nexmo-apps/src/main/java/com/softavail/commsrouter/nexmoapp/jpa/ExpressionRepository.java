package com.softavail.commsrouter.nexmoapp.jpa;

import com.softavail.commsrouter.jpa.GenericRepository;
import com.softavail.commsrouter.jpa.JpaTransactionManager;
import com.softavail.commsrouter.nexmoapp.domain.Expression;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ExpressionRepository extends GenericRepository<Expression> {

  public ExpressionRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

}
