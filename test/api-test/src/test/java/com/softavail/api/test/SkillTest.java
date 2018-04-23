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

import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.softavail.commsrouter.test.api.*;

import com.softavail.commsrouter.api.dto.arg.*;

import com.softavail.commsrouter.api.dto.model.skill.*;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.allOf;


import org.junit.*;

public class SkillTest extends BaseTest {
  private HashMap<CommsRouterResource, String> state = new HashMap<CommsRouterResource, String>();
  private Router r = new Router(state);
  private Skill s = new Skill(state);

  @Before
  public void setup() {
    r.create(new CreateRouterArg());
  }

  @After
  public void cleanup() {
    r.delete();
  }

  @Test
  public void createNoDomain(){
    ApiSkill api_s = new ApiSkill(state);
    api_s.create(state.get(CommsRouterResource.ROUTER), new CreateSkillArg.Builder().description("no domain").build())
      .statusCode(400)
      .body("error.description",is("Field 'domain' is required."));
  }

  @Test
  public void createInvalidSymbols(){
    ApiSkill api_s = new ApiSkill(state);
    api_s.replace(state.get(CommsRouterResource.ROUTER), "with spaces", new CreateSkillArg.Builder()
                 .name("with spaces")
                 .description("invalid name")
                 .domain( new BoolAttributeDomainDto())
                 .build())
      .statusCode(400)
      .body("error.description",is("Invalid selector: with spaces"));
    
  }

  @Test
  public void createMultivalue(){
    s.create(new CreateSkillArg.Builder().description("multivalue").domain( new BoolAttributeDomainDto()).multivalue(true).build());
    SkillDto resource = s.get();
    assertThat(resource.getDescription(), is("multivalue"));
    assertThat(resource.getMultivalue(), is(true));
    s.delete();
  }

  @Test
  public void list(){
    s.create(new CreateSkillArg.Builder().description("bool domain").domain( new BoolAttributeDomainDto()).build());
    List<SkillDto> resource = s.list();
    assertThat(resource.get(0).getDescription(), is("bool domain"));
    assertThat(resource.get(0).getMultivalue(), nullValue());
    s.delete();
  }

  @Test
  public void updateExisting(){
    String id = s.create(new CreateSkillArg.Builder().description("withName").domain( new BoolAttributeDomainDto()).build()).getRef();
    SkillDto resource = s.get();
    assertThat(resource.getDescription(), is("withName"));
    assertThat(resource.getMultivalue(), nullValue());
    String descr = "Replaced description";
    
    s.update(new UpdateSkillArg.Builder().description(descr).multivalue(true).build());
    resource = s.get();
    assertThat(resource.getDescription(), is(descr));
    assertThat(resource.getMultivalue(), is(true));
    s.delete();
  }

  
  @Test
  public void replaceNew(){
    state.put(CommsRouterResource.SKILL, "replaceNewSkill");

    ApiObjectRef oid = s.replace(new CreateSkillArg.Builder().description("withName").domain( new BoolAttributeDomainDto()).build());
    assertThat(oid.getRef(), is("replaceNewSkill"));
    SkillDto resource = s.get();
    assertThat(resource.getDescription(), is("withName"));
    assertThat(resource.getMultivalue(), nullValue());
    s.delete();
  }

  @Test
  public void replaceExisting(){
    state.put(CommsRouterResource.SKILL, "replaceNewSkill");
    String id = s.replace(new CreateSkillArg.Builder().description("withName").domain( new BoolAttributeDomainDto()).build()).getRef();
    SkillDto resource = s.get();
    assertThat(resource.getDescription(), is("withName"));
    assertThat(resource.getMultivalue(), nullValue());
    String descr = "Replaced description";
    
    s.replace(new CreateSkillArg.Builder().description(descr).multivalue(false).domain( new BoolAttributeDomainDto()).build()).getRef();
    resource = s.get();
    assertThat(resource.getDescription(), is(descr));
    assertThat(resource.getMultivalue(), is(false));
    s.delete();
  }

  @Test
  public void createWithDomainNumber() throws com.softavail.commsrouter.api.exception.BadValueException {
    List<NumberInterval> intervals = Stream.of(new NumberInterval(new NumberIntervalBoundary(1.0),new NumberIntervalBoundary(2.0)),
                                       new NumberInterval(new NumberIntervalBoundary(2.0),new NumberIntervalBoundary(3.0)),
                                       new NumberInterval(new NumberIntervalBoundary(4.0,false),new NumberIntervalBoundary(5.0,true))
                                    ).collect(Collectors.toList());
    
    String id= s.create(new CreateSkillArg.Builder()
                        .description("domain")
                        .domain( new NumberAttributeDomainDto(intervals))
                        .multivalue(false)
                        .build()).getRef();
    SkillDto resource = s.get();
    resource.getDomain().validate();
    
    assertThat(resource.getDomain().getType(),is(AttributeType.number));
    assertThat(resource.getMultivalue(), is(false));

    s.delete();
  }

  @Test
  public void createWithDomainEnum(){
    Set<String> options = Stream.of("онче","бонче","cчупено", "пиронче"
                                    //"uno","due","tres"
                                    ).collect(Collectors.toSet());
    
    String id= s.create(new CreateSkillArg.Builder()
                        .description("domain")
                        .domain( new EnumerationAttributeDomainDto(options))
                        .multivalue(false)
                        .build()).getRef();
    SkillDto resource = s.get();
    resource.getDomain().accept(new AttributeDomainDtoVisitor(){
        public void handleEnumerationValues(Set<String> values){
          assertThat(values,is(options));
        }
        public void handleNumberIntervals(List<NumberInterval> intervals){assert(false);}

      public void handleRegex(String regex){assert(false);}
    });
    
    assertThat(resource.getDomain().getType(),is(AttributeType.enumeration));
    assertThat(resource.getMultivalue(), is(false));

    s.delete();
  }

  @Test
  public void createWithBoolean(){
    
    String id= s.create(new CreateSkillArg.Builder()
                        .description("domain")
                        .domain( new BoolAttributeDomainDto())
                        .multivalue(false)
                        .build()).getRef();
    SkillDto resource = s.get();
    
    assertThat(resource.getDomain().getType(),is(AttributeType.bool));
    assertThat(resource.getMultivalue(), is(false));
    s.delete();
  }

  @Test
  public void createWithBooleanMulti(){
    
    String id= s.create(new CreateSkillArg.Builder()
                        .description("domain")
                        .domain( new BoolAttributeDomainDto())
                        .multivalue(true)
                        .build()).getRef();
    SkillDto resource = s.get();
    
    assertThat(resource.getDomain().getType(),is(AttributeType.bool));
    assertThat(resource.getMultivalue(), is(true));
    s.delete();
  }
}
