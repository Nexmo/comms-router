/*
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.softavail.commsrouter.domain;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "attribute_group")
public class AttributeGroup implements Serializable {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @OneToMany(mappedBy = "attributeGroup", cascade = CascadeType.ALL, orphanRemoval = true)
  private List<Attribute> attributes = new ArrayList<>();

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public List<Attribute> getAttributes() {
    return attributes;
  }

  public List<Attribute> getAttributes(String attributeName) {
    List<Attribute> result = new ArrayList<>();
    attributes.stream().filter((attribute) -> (attribute.getName().equals(attributeName)))
        .forEachOrdered((attribute) -> {
          result.add(attribute);
        });
    return result;
  }

  public Attribute getFirstAttribute(String attributeName) {

    for (Attribute attribute : attributes) {
      if (attribute.getName().equals(attributeName)) {
        return attribute;
      }
    }

    return null;
  }

  public void setAttributes(List<Attribute> attributes) {
    this.attributes = attributes;
  }

  public void add(String name, Double value) {
    attributes.add(createAttribute(name, value));
  }

  public void add(String name, String value) {
    attributes.add(createAttribute(name, value));
  }

  public void add(String name, Boolean value) {
    attributes.add(createAttribute(name, value));
  }

  public void addArrayItem(String name, Double value) {
    Attribute attribute = createAttribute(name, value);
    attribute.setIsScalar(Boolean.FALSE);
    attributes.add(attribute);
  }

  public void addArrayItem(String name, String value) {
    Attribute attribute = createAttribute(name, value);
    attribute.setIsScalar(Boolean.FALSE);
    attributes.add(attribute);
  }

  public void addArrayItem(String name, Boolean value) {
    Attribute attribute = createAttribute(name, value);
    attribute.setIsScalar(Boolean.FALSE);
    attributes.add(attribute);
  }

  public Boolean isScalar(String name) {
    Attribute attribute = getFirstAttribute(name);
    if (attribute != null) {
      return attribute.isScalar();
    }
    return null;
  }

  private Attribute createAttribute(String name, Double value) {
    Attribute attribute = new Attribute();
    attribute.setName(name);
    attribute.setDoubleValue(value);
    attribute.setAttributeGroup(this);
    return attribute;
  }

  private Attribute createAttribute(String name, String value) {
    Attribute attribute = new Attribute();
    attribute.setName(name);
    attribute.setStringValue(value);
    attribute.setAttributeGroup(this);
    return attribute;
  }

  private Attribute createAttribute(String name, Boolean value) {
    Attribute attribute = new Attribute();
    attribute.setName(name);
    attribute.setBooleanValue(value);
    attribute.setAttributeGroup(this);
    return attribute;
  }

}
