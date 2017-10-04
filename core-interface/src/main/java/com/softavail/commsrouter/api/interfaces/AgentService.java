package com.softavail.commsrouter.api.interfaces;

import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import com.softavail.commsrouter.api.dto.arg.UpdateAgentArg;
import com.softavail.commsrouter.api.dto.model.AgentDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;

/**
 * Created by @author mapuo on 04.09.17.
 */
public interface AgentService extends RouterObjectService<AgentDto> {

  AgentDto create(CreateAgentArg createArg, RouterObjectId objectId) throws CommsRouterException;

  AgentDto replace(CreateAgentArg createArg, RouterObjectId objectId) throws CommsRouterException;

  void update(UpdateAgentArg updateArg, RouterObjectId objectId) throws CommsRouterException;

}
