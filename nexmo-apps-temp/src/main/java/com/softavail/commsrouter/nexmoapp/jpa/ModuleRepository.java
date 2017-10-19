package com.softavail.commsrouter.nexmoapp.jpa;

import com.softavail.commsrouter.jpa.GenericRepository;
import com.softavail.commsrouter.jpa.JpaTransactionManager;
import com.softavail.commsrouter.nexmoapp.domain.Application;
import com.softavail.commsrouter.nexmoapp.domain.ApplicationModule;
import com.softavail.commsrouter.nexmoapp.domain.Module;
import com.softavail.commsrouter.nexmoapp.dto.model.ModuleRole;
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
  public ApplicationModule getApplicationModuleByRole(EntityManager em,
      String applicationId, ModuleRole moduleRole) {

    String qlString = "SELECT a, m FROM Application a " + "JOIN a.modules m JOIN m.moduleRoles mr "
        + "WHERE a.id = :applicationId AND mr = :moduleRole";

    List<Object[]> result = em.createQuery(qlString).setParameter("applicationId", applicationId)
        .setParameter("moduleRole", moduleRole).getResultList();

    if (result.isEmpty()) {
      return null;
    }

    if (result.size() > 1) {
      LOGGER.error("Application {}: found multiple modules with role {}", applicationId,
          moduleRole);
    }

    Application application = (Application) result.get(0)[0];
    Module module = (Module) result.get(0)[1];

    ApplicationModule applicationModuleForRole = new ApplicationModule();
    applicationModuleForRole.setModuleRole(moduleRole);
    applicationModuleForRole.setApplication(application);
    applicationModuleForRole.setModule(module);
    return applicationModuleForRole;
  }

}
