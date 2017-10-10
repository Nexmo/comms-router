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
public class ArrayOfLongsAttributeValueDto extends ArrayAttributeValueDto<Long> {

  @Override
  public void accept(AttributeValueVisitor visitor) throws IOException {
    visitor.handleArrayOfLongsValue(this);
  }

}
