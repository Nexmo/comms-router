/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.nexmoapp.domain;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "attribute")
public class Attribute implements Serializable {

  @Id
  @GeneratedValue
  private Long id;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "attribute_group_id")
  private AttributeGroup attributeGroup;

  @Column(name = "name")
  private String name;

  @Column(name = "long_value")
  private Long longValue;

  @Column(name = "string_value")
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

  public Long getLongValue() {
    return longValue;
  }

  public void setLongValue(Long longValue) {
    this.longValue = longValue;
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

    public Attribute build(Long value) {
      Attribute attr = new Attribute();
      attr.setName(key);
      attr.setLongValue(value);
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
