/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model.attribute;

import java.io.IOException;

/**
 *
 * @author ikrustev
 */
public class StringAttributeValueDto extends AttributeValueDto {

  private String value;

  public StringAttributeValueDto() {}

  public StringAttributeValueDto(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return value;
  }

  @Override
  public void accept(AttributeValueVisitor visitor) throws IOException {
    visitor.handleStringValue(this);
  }

}
