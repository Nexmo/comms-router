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

import com.softavail.commsrouter.api.dto.model.RouterObjectRef;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;

/**
 * @author ikrustev
 */
@Entity
@Table(name = "skill")
public class Skill extends RouterObject {

  @Column(name = "description")
  @Size(max = 255, message = "{domain.Skill.description.size}")
  private String description;

  @Column(name = "multivalue")
  private Boolean multivalue;

  @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
  @JoinColumn(name = "attribute_domain_id")
  private AttributeDomain domain;

  public Skill() {}

  public Skill(RouterObjectRef objectRef) {
    super(objectRef.getRef());
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public Boolean getMultivalue() {
    return multivalue;
  }

  public void setMultivalue(Boolean multivalue) {
    this.multivalue = multivalue;
  }

  public AttributeDomain getDomain() {
    return domain;
  }

  public void setDomain(AttributeDomain domain) {
    this.domain = domain;
  }

}
