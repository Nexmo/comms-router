package com.softavail.commsrouter.nexmoapp.api.service;

import com.softavail.commsrouter.api.service.CoreApiObjectService;
import com.softavail.commsrouter.nexmoapp.domain.Application;
import com.softavail.commsrouter.nexmoapp.jpa.TransactionManagerFactory;
import com.softavail.commsrouter.nexmoapp.model.ApplicationDto;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ApplicationService
    extends CoreApiObjectService<ApplicationDto, Application> {

  public ApplicationService() {
    super(
        TransactionManagerFactory.getTransactionManager(),
        TransactionManagerFactory.getApplicationRepository(),
        TransactionManagerFactory.getEntityMappers().applicationMapper);
  }

}
