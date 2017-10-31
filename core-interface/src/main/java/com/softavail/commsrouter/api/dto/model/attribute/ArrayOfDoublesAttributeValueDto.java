package com.softavail.commsrouter.api.dto.model.attribute;

import java.io.IOException;

/**
 * Created by @author mapuo on 15.10.17.
 */
public class ArrayOfDoublesAttributeValueDto extends ArrayAttributeValueDto<Double> {

  @Override
  public String getValueAsString() {
    return getValue().toString();
  }

  @Override
  public void accept(AttributeValueVisitor visitor) throws IOException {
    visitor.handleArrayOfDoublesValue(this);
  }

}
