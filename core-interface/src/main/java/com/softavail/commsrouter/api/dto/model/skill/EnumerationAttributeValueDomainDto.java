/*
 * Copyright 2017 - 2018 SoftAvail Inc.
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

package com.softavail.commsrouter.api.dto.model.skill;

import java.util.Set;

/**
 *
 * @author ikrustev
 */
public class EnumerationAttributeValueDomainDto extends AttributeValueDomainDto {

  private Set<String> values;

  public EnumerationAttributeValueDomainDto() {}

  public EnumerationAttributeValueDomainDto(Set<String> values) {
    this.values = values;
  }

  @Override
  public AttributeValueType getType() {
    return AttributeValueType.enumeration;
  }

  @Override
  public void accept(AttributeValueDomainDtoVisitor visitor) {
    visitor.handleEnumerationValues(values);
  }

  public Set<String> getValues() {
    return values;
  }

  public void setValues(Set<String> values) {
    this.values = values;
  }

}
