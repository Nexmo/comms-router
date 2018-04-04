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

import io.restassured.RestAssured;
import io.restassured.specification.RequestSpecification;

import java.util.HashMap;

public class Resource {

  private HashMap<CommsRouterResource, String> state;

  public Resource(HashMap<CommsRouterResource, String> state) {
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();
    java.util.Map<String, String> env = System.getenv();

    if (System.getProperty("autHost") == null) {
      String host = env.get("AUT_HOST");
      if (host != null) {
        RestAssured.baseURI = host;
      } else {
        RestAssured.baseURI = "http://localhost:8080";
      }
    } else {
      // you can specify it using -DautHost=http://localhost:8080
      RestAssured.baseURI = System.getProperty("autHost");
    }
    if (System.getProperty("autPath") == null) {
      String host = env.get("AUT_PATH");
      if (host != null) {
        RestAssured.basePath = host;
      } else {
        RestAssured.basePath = "/comms-router-web/api";
      }
    } else {
      RestAssured.basePath = System.getProperty("autPath");
    }

    this.state = state;
  }

  public HashMap<CommsRouterResource, String> state() {
    return this.state;
  }
  
  protected RequestSpecification req() {
    return given()
      .contentType("application/json");
  }
  
  protected RequestSpecification req(RequestSpecification spec) {
    return given()
      .contentType("application/json")
      .spec(spec);
  }
  
}
