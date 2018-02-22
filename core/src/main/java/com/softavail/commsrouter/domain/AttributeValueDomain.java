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

package com.softavail.commsrouter.domain;

import com.softavail.commsrouter.api.dto.model.skill.AttributeValueType;
import com.softavail.commsrouter.api.dto.model.skill.NumberIntervalBoundary;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.OrderColumn;
import javax.persistence.Table;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "attribute_value_domain")
public class AttributeValueDomain implements Serializable {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "type")
  private AttributeValueType type;

  @OneToMany(mappedBy = "attributeValueDomain", cascade = CascadeType.ALL, orphanRemoval = true)
  @OrderColumn(name = "list_order")
  private List<AttributeValueDefinition> valueDefinitions = new ArrayList<>();

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public AttributeValueType getType() {
    return type;
  }

  public void setType(AttributeValueType type) {
    this.type = type;
  }

  public List<AttributeValueDefinition> getValueDefinitions() {
    return valueDefinitions;
  }

  public void setValueDefinitions(List<AttributeValueDefinition> valueDefinitions) {
    this.valueDefinitions = valueDefinitions;
  }

  public void addEnumValue(String enumValue) {
    AttributeValueDefinition def = new AttributeValueDefinition();
    def.setAttributeValueDomain(this);
    def.setEnumValue(enumValue);
    valueDefinitions.add(def);
  }

  public void addIntervalBoundary(NumberIntervalBoundary intervalBoundary) {
    AttributeValueDefinition def = new AttributeValueDefinition();
    def.setAttributeValueDomain(this);
    def.setBoundary(intervalBoundary.getBoundary());
    def.setInclusive(intervalBoundary.getInclusive());
    valueDefinitions.add(def);
  }

  public void addRegex(String regex) {
    AttributeValueDefinition def = new AttributeValueDefinition();
    def.setAttributeValueDomain(this);
    def.setRegex(regex);
    valueDefinitions.add(def);
  }

}
