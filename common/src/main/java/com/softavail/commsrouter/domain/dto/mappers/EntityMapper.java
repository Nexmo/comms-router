/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.domain.ApiObject;

import java.util.List;
import java.util.stream.Collectors;

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
    return from.stream()
        .map(ApiObject::getId)
        .collect(Collectors.toList());
  }

  protected String getOptionalId(ApiObject apiObject) {
    return apiObject == null ? null : apiObject.getId();
  }

}
