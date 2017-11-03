/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
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

  public void setAttributes(List<Attribute> attributes) {
    this.attributes = attributes;
  }

  public void add(String name, Double value, Boolean scalarValue) {
    Attribute attribute = new Attribute();
    attribute.setName(name);
    attribute.setDoubleValue(value);
    attribute.setScalarValue(scalarValue);
    attribute.setAttributeGroup(this);
    attributes.add(attribute);
  }

  public void add(String name, String value, Boolean scalarValue) {
    Attribute attribute = new Attribute();
    attribute.setName(name);
    attribute.setStringValue(value);
    attribute.setScalarValue(scalarValue);
    attribute.setAttributeGroup(this);
    attributes.add(attribute);
  }

  public void add(String name, Boolean value, Boolean scalarValue) {
    Attribute attribute = new Attribute();
    attribute.setName(name);
    attribute.setBooleanValue(value);
    attribute.setScalarValue(scalarValue);
    attribute.setAttributeGroup(this);
    attributes.add(attribute);
  }

}
