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
public class LongAttributeValueDto extends AttributeValueDto {

  private long value;

  public LongAttributeValueDto() {}

  public LongAttributeValueDto(long value) {
    this.value = value;
  }

  public long getValue() {
    return value;
  }

  public void setValue(long value) {
    this.value = value;
  }

  @Override
  public void accept(AttributeValueVisitor visitor) throws IOException {
    visitor.handleLongValue(this);
  }

}
