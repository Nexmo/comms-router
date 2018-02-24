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

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PostLoad;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import javax.persistence.Table;
import javax.validation.constraints.Size;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "attribute_domain_definition")
public class AttributeDomainDefinition implements Serializable {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "enum_value")
  @Size(max = 255, message = "{domain.AttributeDomainDefinition.value.size}")
  private String enumValue;

  @Column(name = "is_inclusive")
  private Boolean inclusive;

  @Column(name = "boundary")
  private Double boundary;

  @Column(name = "regex")
  @Size(max = 255, message = "{domain.AttributeDomainDefinition.regex.size}")
  private String regex;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "attribute_domain_id")
  private AttributeDomain attributeDomain;

  @PrePersist
  @PreUpdate
  public void storeBoundaryInfinity() {
    if (boundary != null) {
      if (boundary == Double.POSITIVE_INFINITY) {
        boundary = Double.MAX_VALUE;
      } else if (boundary == Double.NEGATIVE_INFINITY) {
        boundary = Double.MIN_VALUE;
      }
    }
  }

  @PostLoad
  public void loadBoundaryInfinity() {
    if (boundary != null) {
      if (boundary == Double.MAX_VALUE) {
        boundary = Double.POSITIVE_INFINITY;
      } else if (boundary == Double.MIN_VALUE) {
        boundary = Double.NEGATIVE_INFINITY;
      }
    }
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public AttributeDomain getAttributeDomain() {
    return attributeDomain;
  }

  public void setAttributeDomain(AttributeDomain attributelValueDomain) {
    this.attributeDomain = attributelValueDomain;
  }

  public String getEnumValue() {
    return enumValue;
  }

  public void setEnumValue(String enumValue) {
    this.enumValue = enumValue;
  }

  public Boolean getInclusive() {
    return inclusive;
  }

  public void setInclusive(Boolean isInclusive) {
    this.inclusive = isInclusive;
  }

  public Double getBoundary() {
    return boundary;
  }

  public void setBoundary(Double boundary) {
    this.boundary = boundary;
  }

  public String getRegex() {
    return regex;
  }

  public void setRegex(String regex) {
    this.regex = regex;
  }

}
