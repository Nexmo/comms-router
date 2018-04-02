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

import io.restassured.builder.RequestSpecBuilder;
import io.restassured.builder.ResponseSpecBuilder;
import io.restassured.response.ValidatableResponse;

import io.restassured.specification.RequestSpecification;
import io.restassured.specification.ResponseSpecification;


import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;

public class ApiRouter extends Resource {

  private static final Logger LOGGER = LogManager.getLogger(ApiRouter.class);

  public ApiRouter(HashMap<CommsRouterResource, String> state) {
      super(state);
  }

  public ValidatableResponse list() {
    return list("");
  }

  public ValidatableResponse list(String query) {
    return req()
      .pathParam("query", query)
      .get("/routers?{query}")
      .then();
  }

  public ValidatableResponse get(String routerRef) {
    return req().pathParam("routerRef", routerRef).get("/routers/{routerRef}").then();
    
  }

  public ValidatableResponse delete(String routerRef) {
    return req()
        .pathParam("routerRef", routerRef)
        .delete("/routers/{routerRef}")
        .then();
  }

  public ValidatableResponse update(String routerRef, CreateRouterArg args) {
    return req()
        .pathParam("routerRef", routerRef)
        .body(args)
        .post("/routers/{routerRef}")
        .then();
  }

  public ValidatableResponse replace(String routerRef, CreateRouterArg args) {
    return req()
        .pathParam("routerRef", routerRef)
        .body(args)
        .put("/routers/{routerRef}")
        .then();
  }

  public ValidatableResponse create(CreateRouterArg args) {
    return req()
      .body(args)
      .post("/routers")
      .then();
  }
}
