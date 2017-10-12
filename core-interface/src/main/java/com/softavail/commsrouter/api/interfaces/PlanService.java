package com.softavail.commsrouter.api.interfaces;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;

/**
 * Created by @author mapuo on 04.09.17.
 */
public interface PlanService extends RouterObjectService<PlanDto> {

  ApiObjectId create(CreatePlanArg createArg, String routerId)
      throws CommsRouterException;

  ApiObjectId create(CreatePlanArg createArg, RouterObjectId objectId)
      throws CommsRouterException;

  void update(UpdatePlanArg updateArg, RouterObjectId objectId)
      throws CommsRouterException;

}
