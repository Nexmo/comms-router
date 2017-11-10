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
public class ArrayOfStringsAttributeValueDto extends ArrayAttributeValueDto<String> {

  @Override
  public String toString() {
    return getValue().toString();
  }

  @Override
  public void accept(AttributeValueVisitor visitor) throws IOException {
    visitor.handleArrayOfStringsValue(this);
  }

}
