package com.softavail.commsrouter.nexmoapp.interfaces;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.ApiObjectService;
import com.softavail.commsrouter.nexmoapp.dto.arg.CreateApplicationArg;
import com.softavail.commsrouter.nexmoapp.dto.arg.UpdateApplicationArg;
import com.softavail.commsrouter.nexmoapp.dto.model.ApplicationDto;

/**
 * Created by @author mapuo on 10.10.17.
 */
public interface ApplicationService extends ApiObjectService<ApplicationDto> {

  ApiObjectId create(CreateApplicationArg createArg)
      throws CommsRouterException;

  ApiObjectId create(CreateApplicationArg createArg, String id)
      throws CommsRouterException;

  void update(UpdateApplicationArg updateArg, String id)
      throws CommsRouterException;

}
