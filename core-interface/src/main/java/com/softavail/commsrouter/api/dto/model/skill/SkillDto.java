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

import com.softavail.commsrouter.api.dto.model.RouterObjectRef;

/**
 *
 * @author ikrustev
 */
public class SkillDto extends RouterObjectRef {

  private String description;
  private Boolean multivalue;
  private AttributeDomainDto domain;

  public SkillDto() {}

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

  public AttributeDomainDto getDomain() {
    return domain;
  }

  public void setDomain(AttributeDomainDto domain) {
    this.domain = domain;
  }

}
