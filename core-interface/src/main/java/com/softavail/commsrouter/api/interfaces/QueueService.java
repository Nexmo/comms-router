/* 
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.commsrouter.api.interfaces;

import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.UpdateQueueArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;

import java.util.Collection;

/**
 * Created by @author mapuo on 05.09.17.
 */
public interface QueueService extends RouterObjectService<QueueDto> {

  ApiObjectId create(CreateQueueArg createArg, String routerId)
      throws CommsRouterException;

  ApiObjectId create(CreateQueueArg createArg, RouterObjectId objectId)
      throws CommsRouterException;

  void update(UpdateQueueArg updateArg, RouterObjectId objectId)
      throws CommsRouterException;

  long getQueueSize(RouterObjectId objectId)
      throws CommsRouterException;

  Collection<TaskDto> getTasks(RouterObjectId objectId)
      throws CommsRouterException;

}
