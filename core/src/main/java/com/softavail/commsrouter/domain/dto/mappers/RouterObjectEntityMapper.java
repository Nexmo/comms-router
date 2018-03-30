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

package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.domain.RouterObject;

/**
 * Created by @author mapuo on 10.10.17.
 */
public abstract class RouterObjectEntityMapper<DTOENTITYT, JPAENTITYT>
    extends EntityMapper<DTOENTITYT, JPAENTITYT> {

  protected void copyRef(RouterObjectRef to, RouterObject from) {
    to.setId(from.getId());
    to.setRef(from.getRef());
    to.setHash(from.hashString());
    to.setRouterRef(from.getRouter().getRef());
  }

}
