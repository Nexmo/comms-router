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

package com.softavail.commsrouter.api.dto.misc;

import java.util.List;

/**
 * Created by @author mapuo on 05.09.17.
 */
public class PaginatedList<ENTITYT> {

  private final List<ENTITYT> list;
  private final String nextToken;

  public PaginatedList(List<ENTITYT> list, String nextToken) {
    this.list = list;
    this.nextToken = nextToken;
  }

  public List<ENTITYT> getList() {
    return list;
  }

  public String getNextToken() {
    return nextToken;
  }

  @Override
  public String toString() {
    return new StringBuilder("PaginatedList{")
        .append("list=").append(list)
        .append(", nextToken='").append(nextToken).append('\'')
        .append('}').toString();
  }

}
