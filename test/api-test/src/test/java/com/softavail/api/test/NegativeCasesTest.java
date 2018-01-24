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

import org.junit.*;
import com.softavail.commsrouter.api.dto.arg.*;
import com.softavail.commsrouter.test.api.*;

import java.util.HashMap;

public class NegativeCasesTest extends BaseTest {
  private static final String longText =  "longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890"+"longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890"+"longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890";
  private static final String utfText =  "longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890";
  
  @Test
  public void createRouterLongName() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    ApiRouter r = new ApiRouter(state);
    r.create(new CreateRouterArg.Builder().name(longText).build())
      .statusCode(500);
  }
  @Test
  public void createRouterLongDescription() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    ApiRouter r = new ApiRouter(state);
    r.create(new CreateRouterArg.Builder().description(longText).build())
      .statusCode(500);
  }
  @Test
  public void createRouterUTFDescription() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    ApiRouter r = new ApiRouter(state);
    r.create(new CreateRouterArg.Builder().description(utfText).build())
      .statusCode(201);
  }
  @Test
  public void replaceRouterLongRefId() {
    HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
    ApiRouter r = new ApiRouter(state);
    r.replace(longText,new CreateRouterArg.Builder().description("longName със символи на кирилица345678901234567890longName със символи на кирилица345678901234567890").build())
      .statusCode(500);
  }

}



