package com.softavail.commsrouter.nexmoapp.domain.dto.mappers;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class EntityMappers {

  public final ApplicationMapper applicationMapper;
  public final SessionMapper sessionMapper;
  public final ModuleMapper expressionMapper;

  public EntityMappers() {
    applicationMapper = new ApplicationMapper();
    expressionMapper = new ModuleMapper();
    sessionMapper = new SessionMapper(applicationMapper, expressionMapper);
  }

}
