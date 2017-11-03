/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model.attribute;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author ikrustev
 * @param <ELEMENT> - the array element
 */
public abstract class ArrayAttributeValueDto<ELEMENT> extends AttributeValueDto {

  private List<ELEMENT> value = new ArrayList<>();

  public ArrayAttributeValueDto() {
    setScalar(false);
  }

  public List<ELEMENT> getValue() {
    return value;
  }

  public void setValue(List<ELEMENT> value) {
    this.value = value;
  }

  public void add(ELEMENT element) {
    value.add(element);
  }
}
