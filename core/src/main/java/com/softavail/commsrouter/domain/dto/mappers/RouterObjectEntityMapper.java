package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.domain.RouterObject;

/**
 * Created by @author mapuo on 10.10.17.
 */
public abstract class RouterObjectEntityMapper<DTOENTITYT, JPAENTITYT>
    extends EntityMapper<DTOENTITYT, JPAENTITYT> {

  protected void copyId(RouterObjectId to, RouterObject from) {
    to.setId(from.getId());
    to.setRouterId(from.getRouterId());
  }

}
