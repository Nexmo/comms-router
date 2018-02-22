/*
 * Copyright 2017 - 2018 SoftAvail Inc.
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

import com.softavail.commsrouter.api.dto.arg.CreateSkillArg;
import com.softavail.commsrouter.api.dto.arg.UpdateSkillArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.skill.SkillDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;

/**
 *
 * @author ikrustev
 */
public interface SkillService extends RouterObjectService<SkillDto> {

  ApiObjectRef create(CreateSkillArg createArg, String routerRef)
      throws CommsRouterException;

  ApiObjectRef replace(CreateSkillArg createArg, RouterObjectRef objectRef)
      throws CommsRouterException;

  void update(UpdateSkillArg updateArg, RouterObjectRef objectRef) throws CommsRouterException;

}
