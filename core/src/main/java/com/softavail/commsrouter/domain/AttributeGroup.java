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

  public Attribute createAttribute(String name, String value) {
    Attribute attribute = new Attribute();
    attribute.setName(name);
    attribute.setStringValue(value);
    attribute.setAttributeGroup(this);
    return attribute;
  }

  public Attribute createAttribute(String name, Boolean value) {
    Attribute attribute = new Attribute();
    attribute.setName(name);
    attribute.setBooleanValue(value);
    attribute.setAttributeGroup(this);
    return attribute;
  }

  public Attribute createAttribute(String name, Double value) {
    Attribute attribute = new Attribute();
    attribute.setName(name);
    attribute.setDoubleValue(value);
    attribute.setAttributeGroup(this);
    return attribute;
  }

  public void add(String name, String value) {
    attributes.add(createAttribute(name, value));
  }

  public void add(String name, Boolean value) {
    attributes.add(createAttribute(name, value));
  }

  public void add(String name, Double value) {
    attributes.add(createAttribute(name, value));
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

  public void addArrayItem(String name, Double value) {
    Attribute attribute = createAttribute(name, value);
    attribute.setIsScalar(Boolean.FALSE);
    attributes.add(attribute);
  }


}
