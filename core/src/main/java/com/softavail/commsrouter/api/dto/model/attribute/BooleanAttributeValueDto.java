/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model.attribute;

import java.io.IOException;

/**
 *
 * @author ergyunsyuleyman
 */
public class BooleanAttributeValueDto extends AttributeValueDto {

  private Boolean value;

  public BooleanAttributeValueDto() {}

  public BooleanAttributeValueDto(Boolean value) {
    this.value = value;
  }

  public Boolean getValue() {
    return value;
  }

  public void setValue(Boolean value) {
    this.value = value;
  }

  @Override
  public void accept(AttributeValueVisitor visitor) throws IOException {
    visitor.handleBooleanValue(this);
  }

}
