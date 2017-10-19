package com.softavail.commsrouter.nexmoapp.api.service;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.service.PaginatedApiObjectService;
import com.softavail.commsrouter.nexmoapp.domain.Application;
import com.softavail.commsrouter.nexmoapp.dto.arg.CreateApplicationArg;
import com.softavail.commsrouter.nexmoapp.dto.arg.UpdateApplicationArg;
import com.softavail.commsrouter.nexmoapp.dto.model.ApplicationDto;
import com.softavail.commsrouter.nexmoapp.interfaces.ApplicationService;
import com.softavail.commsrouter.nexmoapp.jpa.TransactionManagerFactory;
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;

import javax.inject.Inject;
import javax.persistence.EntityManager;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ApplicationServiceImpl
    extends PaginatedApiObjectService<ApplicationDto, Application>
    implements ApplicationService {

  @Inject
  public ApplicationServiceImpl(TransactionManagerFactory factory) {
    super(
        factory.getTransactionManager(),
        factory.getApplicationRepository(),
        factory.getEntityMappers().applicationMapper);
  }

  @Override
  public ApiObjectId create(CreateApplicationArg createArg)
      throws CommsRouterException {

    return transactionManager.execute(em -> {
      ApiObjectId objectId = new ApiObjectId(Uuid.get());
      return create(em, createArg, objectId);
    });
  }

  @Override
  public ApiObjectId create(CreateApplicationArg createArg, String id)
      throws CommsRouterException {

    return transactionManager.execute(em -> {
      repository.delete(em, id);
      return create(em, createArg, new ApiObjectId(id));
    });
  }

  private ApiObjectId create(EntityManager em, CreateApplicationArg arg, ApiObjectId objectId)
      throws CommsRouterException {

    Application application = new Application(arg, objectId);
    em.persist(application);
    ApplicationDto applicationDto = entityMapper.toDto(application);

    return new ApiObjectId(applicationDto);
  }

  @Override
  public void update(UpdateApplicationArg updateArg, String id)
      throws CommsRouterException {

    transactionManager.executeVoid(em -> {
      Application app = repository.get(em, id);
      Fields.update(app::setNexmoAppId, app.getNexmoAppId(), updateArg.getNexmoAppId());
      Fields.update(app::setPublicKey, app.getPublicKey(), updateArg.getPublicKey());
      Fields.update(app::setPrivateKey, app.getPrivateKey(), updateArg.getPrivateKey());
    });
  }

}
