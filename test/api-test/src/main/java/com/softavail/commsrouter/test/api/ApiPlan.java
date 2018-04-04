/*
 * Copyright 2017 SoftAvail, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */


package com.softavail.commsrouter.test.api;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.isEmptyString;
import static org.hamcrest.Matchers.not;


import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;

import io.restassured.builder.RequestSpecBuilder;
import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;

public class ApiPlan extends GResource<CreatePlanArg, UpdatePlanArg> {

  private static final Logger LOGGER = LogManager.getLogger(ApiPlan.class);

  public ApiPlan(HashMap<CommsRouterResource, String> state) {
    super(state,"/routers/{routerRef}/plans");
  }

  public ValidatableResponse list(String routerRef) {
    return list(routerRef, "");
  }

  public ValidatableResponse list(String routerRef, String query) {
    return list(querySpec(routerRef,query));
  }

  public ValidatableResponse get(String routerRef, String planRef) {
    return get(getSpec(routerRef,planRef));
  }

  public ValidatableResponse delete(String routerRef, String planRef) {
    return delete(getSpec(routerRef,planRef));
  }

  public ValidatableResponse create(String routerRef, CreatePlanArg args) {
    return create(createSpec(routerRef), args);
  }

  public ValidatableResponse update(String routerRef, String planRef, UpdatePlanArg args) {
    return update(getSpec(routerRef,planRef), args);
  }

  public ValidatableResponse replace(String routerRef, String planRef, CreatePlanArg args) {
    return replace(getSpec(routerRef, planRef), args);
  }

}
