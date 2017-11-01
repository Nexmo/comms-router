package com.softavail.commsrouter.api.dto.model.attribute;

import java.io.IOException;

/**
 * Created by @author mapuo on 15.10.17.
 */
public class DoubleAttributeValueDto extends AttributeValueDto {

  private double value;

  public DoubleAttributeValueDto() {}

  public DoubleAttributeValueDto(double value) {
    this.value = value;
  }

  public double getValue() {
    return value;
  }

  public void setValue(double value) {
    this.value = value;
  }

  @Override
  public String getValueAsString() {
    return Double.toString(getValue());
  }

  @Override
  public void accept(AttributeValueVisitor visitor) throws IOException {
    visitor.handleDoubleValue(this);
  }

}
