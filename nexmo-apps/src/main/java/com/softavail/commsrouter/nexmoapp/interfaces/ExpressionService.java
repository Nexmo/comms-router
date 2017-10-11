package com.softavail.commsrouter.nexmoapp.interfaces;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.PaginatedService;
import com.softavail.commsrouter.nexmoapp.dto.arg.CreateExpressionArg;
import com.softavail.commsrouter.nexmoapp.dto.arg.UpdateExpressionArg;
import com.softavail.commsrouter.nexmoapp.dto.model.ExpressionDto;

/**
 * Created by @author mapuo on 10.10.17.
 */
public interface ExpressionService extends PaginatedService<ExpressionDto> {

  ApiObjectId create(CreateExpressionArg createArg)
      throws CommsRouterException;

  ApiObjectId create(CreateExpressionArg createArg, String id)
      throws CommsRouterException;

  void update(UpdateExpressionArg updateArg, String id)
      throws CommsRouterException;

}
