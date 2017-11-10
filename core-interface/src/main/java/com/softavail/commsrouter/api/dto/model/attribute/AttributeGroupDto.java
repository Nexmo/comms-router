/* 
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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

  public <T extends ArrayAttributeValueDto> T get(Class<T> ct, String name) {
    T tv = ct.cast(get(name));
    if (tv != null) {
      return tv;
    }
    try {
      tv = ct.newInstance();
    } catch (InstantiationException | IllegalAccessException ex) {
      throw new RuntimeException("Unexpected ArrayAttributeValueDto instantiation exception", ex);
    }
    put(name, tv);
    return tv;
  }

}
