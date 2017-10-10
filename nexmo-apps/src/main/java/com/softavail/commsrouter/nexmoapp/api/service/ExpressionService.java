package com.softavail.commsrouter.nexmoapp.api.service;

import com.softavail.commsrouter.api.service.CoreApiObjectService;
import com.softavail.commsrouter.nexmoapp.domain.Expression;
import com.softavail.commsrouter.nexmoapp.jpa.TransactionManagerFactory;
import com.softavail.commsrouter.nexmoapp.model.ExpressionDto;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ExpressionService extends CoreApiObjectService<ExpressionDto, Expression> {

  public ExpressionService() {
    super(TransactionManagerFactory.getTransactionManager(),
        TransactionManagerFactory.getExpressionRepository(),
        TransactionManagerFactory.getEntityMappers().expressionMapper);
  }

}
