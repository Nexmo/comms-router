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

  public void add(String name, String value) {
    AttributeValueDto old = put(name, new StringAttributeValueDto(value));
    assert old == null;
  }

  public void add(String name, Double value) {
    AttributeValueDto old = put(name, new DoubleAttributeValueDto(value));
    assert old == null;
  }

  public void add(String name, Boolean value) {
    AttributeValueDto old = put(name, new BooleanAttributeValueDto(value));
    assert old == null;
  }

  public void addToArray(String name, String value) {
    get(ArrayOfStringsAttributeValueDto.class, name).add(value);
  }

  public void addToArray(String name, Double value) {
    get(ArrayOfDoublesAttributeValueDto.class, name).add(value);
  }

  public void addToArray(String name, Boolean value) {
    get(ArrayOfBooleansAttributeValueDto.class, name).add(value);
  }

  public <T extends ArrayAttributeValueDto> T get(Class<T> c, String name) {
    T t = c.cast(get(name));
    if (t != null) {
      return t;
    }
    try {
      t = c.newInstance();
    } catch (InstantiationException | IllegalAccessException ex) {
      throw new RuntimeException("Unexpected ArrayAttributeValueDto instantiation exception", ex);
    }
    put(name, t);
    return t;
  }

}
