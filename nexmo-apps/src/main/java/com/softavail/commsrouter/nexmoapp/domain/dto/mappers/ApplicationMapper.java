package com.softavail.commsrouter.nexmoapp.domain.dto.mappers;

import com.softavail.commsrouter.domain.dto.mappers.EntityMapper;
import com.softavail.commsrouter.nexmoapp.domain.Application;
import com.softavail.commsrouter.nexmoapp.dto.model.ApplicationDto;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ApplicationMapper extends EntityMapper<ApplicationDto, Application> {

  @Override
  public ApplicationDto toDto(Application jpa) {
    return new ApplicationDto(jpa.getId(), jpa.getNexmoAppId(), jpa.getPublicKey());
  }

}
