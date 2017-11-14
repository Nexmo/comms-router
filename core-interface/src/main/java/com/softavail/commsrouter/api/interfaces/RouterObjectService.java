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

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;

import java.util.List;

/**
 * Created by @author mapuo on 04.09.17.
 */
public interface RouterObjectService<ENTITYT extends RouterObjectId> {

  String TOTAL_COUNT_HEADER = "X-Total-Count";
  String PAGE_NUMBER_PARAM = "page_num";
  String ITEMS_PER_PAGE_PARAM = "per_page";

  ENTITYT get(RouterObjectId routerObjectId)
      throws CommsRouterException;

  List<ENTITYT> list(String routerId)
      throws CommsRouterException;

  PaginatedList<ENTITYT> list(String routerId, int page, int perPage)
      throws CommsRouterException;

  void delete(RouterObjectId routerObjectId)
      throws CommsRouterException;

}
