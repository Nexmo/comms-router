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

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;

import io.restassured.builder.RequestSpecBuilder;
import io.restassured.builder.ResponseSpecBuilder;
import io.restassured.response.ValidatableResponse;

import io.restassured.specification.RequestSpecification;
import io.restassured.specification.ResponseSpecification;


import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;

public class ApiRouter extends GResource<CreateRouterArg, UpdateRouterArg> {

  private static final Logger LOGGER = LogManager.getLogger(ApiRouter.class);

  public ApiRouter(HashMap<CommsRouterResource, String> state) {
    super(state,"/routers");
  }

  public ValidatableResponse list() {
    return list("");
  }

  public ValidatableResponse list(String query) {
    return list(querySpec(query));
  }

  public ValidatableResponse get(String routerRef) {
    return get(getSpec(routerRef));
  }

  public ValidatableResponse delete(String routerRef) {
    return delete(getSpec(routerRef));
  }

  public ValidatableResponse update(String routerRef, UpdateRouterArg args) {
    return update(getSpec(routerRef),args);
  }

  public ValidatableResponse replace(String routerRef, CreateRouterArg args) {
    return replace(getSpec(routerRef),args);
  }

  public ValidatableResponse create(CreateRouterArg args) {
    return create(createSpec(),args);
  }
}
