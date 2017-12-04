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

import com.softavail.commsrouter.api.dto.model.ApiObjectRef;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Table;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "router")
public class Router extends ApiObject {

  private String name;
  private String description;


  @OneToOne(mappedBy = "router", cascade = CascadeType.ALL, fetch = FetchType.LAZY,
          optional = true, orphanRemoval = true)
  private RouterConfig config;

  public Router() {}

  public Router(ApiObjectRef objectId) {
    super(objectId.getRef());
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public RouterConfig getConfig() {
    return config;
  }

  public void setConfig(RouterConfig config) {
    this.config = config;
  }

  @Override
  public String toString() {
    return "Router: [" + "ref=" + getRef() + "]";
  }

}
