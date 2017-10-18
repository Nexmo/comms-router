/*
 * To change this license header, choose License Headers in Project AttributeGroupDto. To change
 * this template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model.attribute;

import java.util.HashMap;

/**
 *
 * @author ikrustev
 */
public class AttributeGroupDto extends HashMap<String, AttributeValueDto> {

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Attributes [ ");
    entrySet().stream().forEach(e -> {
      sb.append(e.getKey()).append(":").append(e.getValue()).append(" ");
    });
    sb.append("]");
    return sb.toString();
  }

  public AttributeGroupDto withKeyValue(String key, AttributeValueDto value) {
    this.put(key,value);
    return this;
  }

}
