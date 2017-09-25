/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.domain.Queue;

/**
 *
 * @author ikrustev
 */
public class QueueMapper extends EntityMapper<QueueDto, Queue> {

  public QueueDto toDto(Queue jpa) {
    QueueDto dto = new QueueDto();
    copyId(dto, jpa);
    dto.setDescription(jpa.getDescription());
    dto.setPredicate(jpa.getPredicate());
    dto.setAgentIds(createIdList(jpa.getAgents()));
    return dto;
  }

}
