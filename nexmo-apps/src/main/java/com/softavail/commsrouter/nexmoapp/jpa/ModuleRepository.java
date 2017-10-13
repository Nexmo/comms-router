package com.softavail.commsrouter.nexmoapp.jpa;

import com.softavail.commsrouter.jpa.GenericRepository;
import com.softavail.commsrouter.jpa.JpaTransactionManager;
import com.softavail.commsrouter.nexmoapp.domain.Application;
import com.softavail.commsrouter.nexmoapp.domain.ApplicationEntryPointModule;
import com.softavail.commsrouter.nexmoapp.domain.Module;
import com.softavail.commsrouter.nexmoapp.dto.model.EntryPoint;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import javax.persistence.EntityManager;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ModuleRepository extends GenericRepository<Module> {

  private static final Logger LOGGER = LogManager.getLogger(ModuleRepository.class);

  public ModuleRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

  @SuppressWarnings("unchecked")
  public ApplicationEntryPointModule getApplicationEntryPoint(EntityManager em,
      String applicationId, EntryPoint entryPoint) {

    String qlString = "SELECT a, m FROM Application a "
        + "JOIN a.modules m JOIN m.entryPoints ep "
        + "WHERE a.id = :applicationId AND ep = :entryPoint";

    List<Object[]> result = em.createQuery(qlString)
        .setParameter("applicationId", applicationId)
        .setParameter("entryPoint", entryPoint)
        .getResultList();

    if (result.isEmpty()) {
      return null;
    }

    if (result.size() > 1) {
      LOGGER.error("Application {}: found multiple entry points of type {}", applicationId,
          entryPoint);
    }

    Application application = (Application) result.get(0)[0];
    Module module = (Module) result.get(0)[1];

    ApplicationEntryPointModule applicationEntryPointModule = new ApplicationEntryPointModule();
    applicationEntryPointModule.setEntryPoint(entryPoint);
    applicationEntryPointModule.setApplication(application);
    applicationEntryPointModule.setModule(module);
    return applicationEntryPointModule;
  }

}
