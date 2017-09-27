package com.softavail.commsrouter.api.interfaces;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;

import java.util.Collection;

/**
 * Created by @author mapuo on 05.09.17.
 */
public interface QueueService extends RouterObjectService<QueueDto> {

  QueueDto create(CreateQueueArg createArg, RouterObjectId objectId) throws CommsRouterException;

  QueueDto put(CreateQueueArg createArg, RouterObjectId objectId) throws CommsRouterException;

  void update(UpdateQueueArg updateArg, RouterObjectId objectId) throws CommsRouterException;

  long getQueueSize(RouterObject routerObjectId) throws CommsRouterException;

  Collection<TaskDto> getTasks(RouterObject routerObjectId) throws CommsRouterException;

}
