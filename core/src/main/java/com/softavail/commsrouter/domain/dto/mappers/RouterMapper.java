/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.domain.Router;

/**
 *
 * @author ikrustev
 */
public class RouterMapper extends EntityMapper<RouterDto, Router> {

  public RouterDto toDto(Router jpa) {
    RouterDto dto = new RouterDto();
    dto.setId(jpa.getId());
    dto.setName(jpa.getName());
    dto.setDescription(jpa.getDescription());
    return dto;
  }

}
