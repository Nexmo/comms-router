/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
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

  public static class Builder {

    private String key;

    public Builder() {}

    public Builder setKey(String key) {
      this.key = key;
      return this;
    }

    public Attribute build(Double value) {
      Attribute attr = new Attribute();
      attr.setName(key);
      attr.setDoubleValue(value);
      return attr;
    }

    public Attribute build(String value) {
      Attribute attr = new Attribute();
      attr.setName(key);
      attr.setStringValue(value);
      return attr;
    }

    public Attribute build(Boolean value) {
      Attribute attr = new Attribute();
      attr.setName(key);
      attr.setBooleanValue(value);
      return attr;
    }

  }

  public static Builder builder() {
    return new Builder();
  }

}
