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

import com.softavail.commsrouter.api.dto.arg.CreateSkillArg;
import com.softavail.commsrouter.api.dto.arg.UpdateSkillArg;

import io.restassured.response.ValidatableResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;

public class ApiSkill extends GResource<CreateSkillArg, UpdateSkillArg> {

  private static final Logger LOGGER = LogManager.getLogger(ApiSkill.class);

  public ApiSkill(HashMap<CommsRouterResource, String> state) {
    super(state,"/routers/{routerRef}/skills");
  }

  public ValidatableResponse list(String routerRef) {
    return list(routerRef,"");
  }
  
  public ValidatableResponse list(String routerRef, String query) {
    return list(querySpec(routerRef,query));
  }

  public ValidatableResponse get(String routerRef, String skillRef) {
    return get(getSpec(routerRef, skillRef));
  }

  public ValidatableResponse delete(String routerRef, String skillRef) {
    return delete(getSpec(routerRef, skillRef));
  }

  public ValidatableResponse create(String routerRef, CreateSkillArg args) {
    return create(createSpec(routerRef), args);
  }

  public ValidatableResponse update(String routerRef, String skillRef, UpdateSkillArg args) {
    return update(getSpec(routerRef, skillRef), args);
  }

  public ValidatableResponse replace(String routerRef, String skillRef, CreateSkillArg args) {
    return replace(getSpec(routerRef, skillRef), args);
  }

}
