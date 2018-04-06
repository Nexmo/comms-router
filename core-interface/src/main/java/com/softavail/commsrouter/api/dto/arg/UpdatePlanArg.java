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

package com.softavail.commsrouter.api.dto.arg;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.RuleDto;

import java.util.List;

/**
 *
 * @author ikrustev
 */
public class UpdatePlanArg {

  private String description;
  private List<RuleDto> rules;
  private RouteDto defaultRoute;

  @JsonIgnore
  public boolean canDoNoBackupUpdate() {
    return getRules() == null && getDefaultRoute() == null;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public List<RuleDto> getRules() {
    return rules;
  }

  public void setRules(List<RuleDto> rules) {
    this.rules = rules;
  }

  public RouteDto getDefaultRoute() {
    return defaultRoute;
  }

  public void setDefaultRoute(RouteDto defaultRoute) {
    this.defaultRoute = defaultRoute;
  }

}
