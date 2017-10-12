package com.softavail.commsrouter.nexmoapp.api.service;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.service.PaginatedApiObjectService;
import com.softavail.commsrouter.nexmoapp.domain.Module;
import com.softavail.commsrouter.nexmoapp.dto.arg.CreateExpressionArg;
import com.softavail.commsrouter.nexmoapp.dto.arg.UpdateExpressionArg;
import com.softavail.commsrouter.nexmoapp.dto.model.ModuleDto;
import com.softavail.commsrouter.nexmoapp.interfaces.ModuleService;
import com.softavail.commsrouter.nexmoapp.jpa.TransactionManagerFactory;
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;

import javax.persistence.EntityManager;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ModuleServiceImpl
    extends PaginatedApiObjectService<ModuleDto, Module>
    implements ModuleService {

  public ModuleServiceImpl() {
    super(TransactionManagerFactory.getTransactionManager(),
        TransactionManagerFactory.getExpressionRepository(),
        TransactionManagerFactory.getEntityMappers().expressionMapper);
  }

  @Override
  public ApiObjectId create(CreateExpressionArg createArg)
      throws CommsRouterException {

    return transactionManager.execute(em -> {
      ApiObjectId objectId = new ApiObjectId(Uuid.get());
      return create(em, createArg, objectId);
    });
  }

  @Override
  public ApiObjectId create(CreateExpressionArg createArg, String id)
      throws CommsRouterException {

    return transactionManager.execute(em -> {
      repository.delete(em, id);
      return create(em, createArg, new ApiObjectId(id));
    });
  }

  private ApiObjectId create(EntityManager em, CreateExpressionArg arg, ApiObjectId objectId)
      throws CommsRouterException {

    Module expression = new Module(arg, objectId);
    em.persist(expression);
    ModuleDto expressionDto = entityMapper.toDto(expression);

    return new ApiObjectId(expressionDto);
  }

  @Override
  public void update(UpdateExpressionArg updateArg, String id)
      throws CommsRouterException {

    transactionManager.executeVoid(em -> {
      Module expression = repository.get(em, id);
      Fields.update(expression::setProgram, expression.getProgram(), updateArg.getProgram());
    });
  }
}
