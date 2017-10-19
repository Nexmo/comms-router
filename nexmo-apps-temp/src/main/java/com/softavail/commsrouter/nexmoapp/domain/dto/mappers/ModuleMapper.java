package com.softavail.commsrouter.nexmoapp.domain.dto.mappers;

import com.softavail.commsrouter.domain.dto.mappers.EntityMapper;
import com.softavail.commsrouter.nexmoapp.domain.Module;
import com.softavail.commsrouter.nexmoapp.dto.model.ModuleDto;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ModuleMapper extends EntityMapper<ModuleDto, Module> {

  @Override
  public ModuleDto toDto(Module jpa) {
    return new ModuleDto(jpa.getId(), jpa.getProgram());
  }

}
