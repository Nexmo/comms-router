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

import com.softavail.commsrouter.api.dto.model.RouteDto;
import com.softavail.commsrouter.api.dto.model.RuleDto;

import java.util.List;

/**
 *
 * @author ikrustev
 */
public class CreatePlanArg {

  private String description;
  private List<RuleDto> rules;
  private RouteDto defaultRoute;


  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  /**
   * @return the rules
   */
  public List<RuleDto> getRules() {
    return rules;
  }

  /**
   * @param rules the rules to set
   */
  public void setRules(List<RuleDto> rules) {
    this.rules = rules;
  }

  public RouteDto getDefaultRoute() {
    return defaultRoute;
  }

  public void setDefaultRoute(RouteDto defaultRoute) {
    this.defaultRoute = defaultRoute;
  }

  public static class Builder{
    private CreatePlanArg planArg = new CreatePlanArg();

    public Builder(String description) {
      planArg.setDescription(description);
    }

    public Builder rules(List<RuleDto> rules) {
      planArg.setRules(rules);
      return this;
    }

    public Builder defaultRoute(RouteDto defaultRoute) {
      planArg.setDefaultRoute(defaultRoute);
      return this;
    }

    public CreatePlanArg build() {
      return planArg;
    }

  }

}
