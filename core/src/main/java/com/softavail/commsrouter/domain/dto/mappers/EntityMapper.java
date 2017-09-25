/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.dto.model.RouterObject;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author ikrustev
 */
public abstract class EntityMapper<DTOENTITYT, JPAENTITYT> {

  public abstract DTOENTITYT toDto(JPAENTITYT jpa);

  public List<DTOENTITYT> toDto(List<JPAENTITYT> jpaList) {
    List<DTOENTITYT> dtoList = new ArrayList<>();
    jpaList.stream().forEach(jpaEntity -> dtoList.add(EntityMapper.this.toDto(jpaEntity)));
    return dtoList;
  }

  protected void copyId(RouterObject to, RouterObject from) {
    to.setId(from.getId());
    to.setRouterId(from.getRouterId());
  }

  protected <ELEMENT extends ApiObject> List<String> createIdList(List<ELEMENT> from) {
    List<String> ids = new ArrayList<>();
    from.stream().forEach(o -> ids.add(o.getId()));
    return ids;
  }

  protected String getOptionalId(ApiObject apiObject) {
    return apiObject == null ? null : apiObject.getId();
  }

}
