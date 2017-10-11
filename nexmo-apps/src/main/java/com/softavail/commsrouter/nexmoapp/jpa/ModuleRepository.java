package com.softavail.commsrouter.nexmoapp.jpa;

import com.softavail.commsrouter.jpa.GenericRepository;
import com.softavail.commsrouter.jpa.JpaTransactionManager;
import com.softavail.commsrouter.nexmoapp.domain.Module;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ModuleRepository extends GenericRepository<Module> {

  public ModuleRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

}
