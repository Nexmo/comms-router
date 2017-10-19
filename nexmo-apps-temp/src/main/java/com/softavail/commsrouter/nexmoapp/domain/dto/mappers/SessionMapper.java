package com.softavail.commsrouter.nexmoapp.domain.dto.mappers;

import com.softavail.commsrouter.domain.dto.mappers.EntityMapper;
import com.softavail.commsrouter.nexmoapp.domain.Session;
import com.softavail.commsrouter.nexmoapp.dto.model.SessionDto;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class SessionMapper extends EntityMapper<SessionDto, Session> {

  private final ApplicationMapper applicationMapper;
  private final ModuleMapper expressionMapper;

  public SessionMapper(ApplicationMapper applicationMapper, ModuleMapper expressionMapper) {
    this.applicationMapper = applicationMapper;
    this.expressionMapper = expressionMapper;
  }

  @Override
  public SessionDto toDto(Session jpa) {
    return new SessionDto(
        jpa.getId(),
        applicationMapper.toDto(jpa.getApplication()),
        expressionMapper.toDto(jpa.getCurrentModule()));
  }

}
