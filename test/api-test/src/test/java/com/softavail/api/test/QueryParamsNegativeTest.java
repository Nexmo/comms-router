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

package com.softavail.api.test;

import com.softavail.commsrouter.test.api.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.beans.HasPropertyWithValue.hasProperty;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.*;
import static io.restassured.RestAssured.*;
import io.restassured.response.ValidatableResponse;

import com.softavail.commsrouter.api.dto.arg.*;
import com.softavail.commsrouter.api.dto.model.*;
import com.softavail.commsrouter.test.api.*;


import org.junit.Test;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

/** Unit test for simple App. */
public class QueryParamsNegativeTest extends BaseTest {

  @Test
  public void perPage() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    ApiObjectRef ref = r.create(new CreateRouterArg());
    ApiRouter api_r = new ApiRouter(state);
    api_r.list("per_page=10000").statusCode(400)
      .body("violations[0].violation", is("We can't serve that many items per page"));
  }

  @Test
  public void perPage0() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    ApiObjectRef ref = r.create(new CreateRouterArg());
    ApiRouter api_r = new ApiRouter(state);
    api_r.list("per_page=0").statusCode(400)
      .body("violations[0].violation", is("You should get at least one item per page"));
  }

  @Test
  public void invalidExpression() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    ApiObjectRef ref = r.create(new CreateRouterArg());
    ApiRouter api_r = new ApiRouter(state);
    api_r.list("q=garbage").statusCode(500)
      .body("error.description", is("cz.jirutka.rsql.parser.ParseException: Encountered \"<EOF>\" at line 1, column 7.\nWas expecting one of:\n    <COMP_FIQL> ...\n    <COMP_ALT> ...\n    "));
  }

  @Test
  public void invalidProperty() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    ApiObjectRef ref = r.create(new CreateRouterArg());
    ApiRouter api_r = new ApiRouter(state);
    api_r.list("q=invalid_property==60").statusCode(400)
      .body("error.description", is("Unknown property: invalid_property from entity com.softavail.commsrouter.domain.Router"));
  }

  @Test
  public void invalidSortProperty() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    Router r = new Router(state);
    ApiObjectRef ref = r.create(new CreateRouterArg());
    ApiRouter api_r = new ApiRouter(state);
    api_r.list("sort=no_leading_sign").statusCode(400)
      .body("violations[0].violation", is("Sort params should start with - or + followed by property name and can't be more than 3"));
  }
  
}
