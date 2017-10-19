package com.softavail.commsrouter.nexmoapp.domain.dto.mappers;

import com.softavail.commsrouter.domain.dto.mappers.AttributesMapper;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class EntityMappers {

  public final ApplicationMapper applicationMapper;
  public final SessionMapper sessionMapper;
  public final ModuleMapper expressionMapper;
  public final AttributesMapper attributesMapper;

  public EntityMappers() {
    applicationMapper = new ApplicationMapper();
    expressionMapper = new ModuleMapper();
    sessionMapper = new SessionMapper(applicationMapper, expressionMapper);
    attributesMapper = new AttributesMapper();
  }

}
