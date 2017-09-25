package com.softavail.commsrouter.api.interfaces;

import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskArg;
import com.softavail.commsrouter.api.dto.arg.UpdateTaskContext;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;

/**
 * Created by @author mapuo on 04.09.17.
 */
public interface TaskService extends RouterObjectService<TaskDto> {

  TaskDto create(CreateTaskArg createArg) throws CommsRouterException;

  void update(UpdateTaskArg updateArg) throws CommsRouterException;

  void update(UpdateTaskContext taskContext) throws CommsRouterException;

}
