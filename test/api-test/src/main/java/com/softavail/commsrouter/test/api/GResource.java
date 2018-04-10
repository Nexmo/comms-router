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
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;

import java.util.HashMap;
import javax.ws.rs.core.HttpHeaders;

public class GResource<C,U> {

  private HashMap<CommsRouterResource, String> state;
  private String prefix = "";
  

  public SpecInfo getSpecEtag(String etag, String itemRef) {
    return new SpecInfo(new RequestSpecBuilder()
                        .addHeader(HttpHeaders.IF_MATCH, etag)
                        .addPathParam("itemRef",itemRef).build(),
                        prefix + "/{itemRef}");
  }

  
  public SpecInfo querySpec(String query) {
    return new SpecInfo(new RequestSpecBuilder()
                        .addPathParam("query",query)
                        .build(),
                        prefix + "?{query}");
  }

  public SpecInfo querySpec(String routerRef, String query) {
    return new SpecInfo(new RequestSpecBuilder()
                        .addPathParam("query",query)
                        .addPathParam("routerRef",routerRef).build(),
                        prefix + "?{query}");
  }
  
  public SpecInfo getSpec(String itemRef) {
    return new SpecInfo(new RequestSpecBuilder()
                        .addPathParam("itemRef",itemRef).build(),
                        prefix + "/{itemRef}");
  }

  public SpecInfo getSpec(String routerRef, String itemRef) {
    return new SpecInfo(new RequestSpecBuilder()
                        .addPathParam("itemRef",itemRef)
                        .addPathParam("routerRef",routerRef).build(),
                        prefix + "/{itemRef}");
  }
  
  public SpecInfo getSpec(String etag, String routerRef, String itemRef) {
    return new SpecInfo(new RequestSpecBuilder()
                        .addHeader(HttpHeaders.IF_MATCH, etag)
                        .addPathParam("itemRef",itemRef)
                        .addPathParam("routerRef",routerRef).build(),
                        prefix + "/{itemRef}");
  }

  public SpecInfo createSpec() {
    return new SpecInfo(new RequestSpecBuilder().build(),
                        prefix);
  }
  
  public SpecInfo createSpec(String routerRef) {
    return new SpecInfo(new RequestSpecBuilder()
                        .addPathParam("routerRef",routerRef).build(),
                        prefix);
  }
  
  public GResource(HashMap<CommsRouterResource, String> state, String prefix) {
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
    this.prefix = prefix;
  }

  public HashMap<CommsRouterResource, String> state() {
    return this.state;
  }
  
  protected RequestSpecification req() {
    return given()
      .contentType("application/json");
  }
  
  protected RequestSpecification req(RequestSpecification spec) {
    return req().spec(spec);
  }

  public ValidatableResponse list(SpecInfo specInfo) {
    return req(specInfo.spec()).get(specInfo.url()).then();
  }

  public ValidatableResponse get(SpecInfo specInfo) {
    return req(specInfo.spec()).get(specInfo.url()).then();
  }

  public ValidatableResponse delete(SpecInfo specInfo) {
    return req(specInfo.spec()).delete(specInfo.url()).then();
  }

  public ValidatableResponse create(SpecInfo specInfo, C args) {
    return req(specInfo.spec()).body(args).post(specInfo.url()).then();
  }

  public ValidatableResponse update(SpecInfo specInfo, U args) {
    return req(specInfo.spec()).body(args).post(specInfo.url()).then();
  }

  public ValidatableResponse replace(SpecInfo specInfo, C args) {
    return req(specInfo.spec()).body(args).put(specInfo.url()).then();
  }
  
}
