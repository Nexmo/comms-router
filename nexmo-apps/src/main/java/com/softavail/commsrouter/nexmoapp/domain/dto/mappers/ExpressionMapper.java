package com.softavail.commsrouter.nexmoapp.domain.dto.mappers;

import com.softavail.commsrouter.domain.dto.mappers.EntityMapper;
import com.softavail.commsrouter.nexmoapp.domain.Expression;
import com.softavail.commsrouter.nexmoapp.model.ExpressionDto;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class ExpressionMapper extends EntityMapper<ExpressionDto, Expression> {

  @Override
  public ExpressionDto toDto(Expression jpa) {
    return new ExpressionDto(jpa.getId(), jpa.getProgram());
  }

}
