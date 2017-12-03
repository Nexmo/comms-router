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

import com.softavail.commsrouter.domain.ApiObject;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author ikrustev
 */
public abstract class EntityMapper<DTOENTITYT, JPAENTITYT> {

  public abstract DTOENTITYT toDto(JPAENTITYT jpa);

  public List<DTOENTITYT> toDto(List<JPAENTITYT> jpaList) {
    return jpaList.stream()
        .map(this::toDto)
        .collect(Collectors.toList());
  }

  protected <ELEMENT extends ApiObject> List<String> createIdList(List<ELEMENT> from) {
    return createIdList(from.stream());
  }

  protected <ELEMENT extends ApiObject> List<String> createIdList(Stream<ELEMENT> stream) {
    return stream
        .map(ApiObject::getRef)
        .collect(Collectors.toList());
  }

  protected String getOptionalId(ApiObject apiObject) {
    return apiObject == null ? null : apiObject.getRef();
  }

}
