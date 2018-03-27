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

package com.softavail.commsrouter.domain;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "attribute")
public class Attribute implements Serializable {

  public static enum Type {
    STRING, DOUBLE, BOOLEAN
  }

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "attribute_group_id")
  private AttributeGroup attributeGroup;

  @Column(name = "name")
  @Size(max = 255, message = "{domain.Attribute.name.size}")
  private String name;

  @Column(name = "double_value")
  private Double doubleValue;

  @Column(name = "string_value")
  @Size(max = 255, message = "{domain.Attribute.string.size}")
  private String stringValue;

  @Column(name = "boolean_value")
  private Boolean booleanValue;

  @Column(name = "is_scalar", columnDefinition = "boolean default true", nullable = false)
  private Boolean isScalar = true;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public AttributeGroup getAttributeGroup() {
    return attributeGroup;
  }

  public void setAttributeGroup(AttributeGroup attributeGroup) {
    this.attributeGroup = attributeGroup;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public Double getDoubleValue() {
    return doubleValue;
  }

  public void setDoubleValue(Double doubleValue) {
    this.doubleValue = doubleValue;
  }

  public String getStringValue() {
    return stringValue;
  }

  public void setStringValue(String stringValue) {
    this.stringValue = stringValue;
  }

  public Boolean getBooleanValue() {
    return booleanValue;
  }

  public void setBooleanValue(Boolean booleanValue) {
    this.booleanValue = booleanValue;
  }

  public Boolean isScalar() {
    return isScalar;
  }

  public void setIsScalar(Boolean isScalar) {
    this.isScalar = isScalar;
  }

  public Object getValue() {
    return doubleValue != null ? doubleValue : (booleanValue != null ? booleanValue : stringValue);
  }

  public Type getType() {
    if (getStringValue() != null) {
      assert getBooleanValue() == null && getDoubleValue() == null;
      return Type.STRING;
    }
    if (getDoubleValue() != null) {
      assert getBooleanValue() == null && getStringValue() == null;
      return Type.DOUBLE;
    }
    if (getBooleanValue() != null) {
      assert getStringValue() == null && getDoubleValue() == null;
      return Type.BOOLEAN;
    }
    throw new RuntimeException("Attribute with no value: " + getId() + " / " + getName());
  }

  public String toString() {
    return "(" + name + ":" + getValue() + ")";
  }
}
