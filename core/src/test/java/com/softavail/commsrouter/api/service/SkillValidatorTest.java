/*
 * Copyright 2018 SoftAvail Inc.
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
package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreateSkillArg;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.skill.AttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.BoolAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.EnumerationAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberInterval;
import com.softavail.commsrouter.api.dto.model.skill.NumberIntervalBoundary;
import com.softavail.commsrouter.api.dto.model.skill.StringAttributeDomainDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.domain.dto.mappers.AttributeDomainMapper;
import com.softavail.commsrouter.jpa.test.TestBase;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import jersey.repackaged.com.google.common.collect.Lists;
import org.junit.Before;
import org.junit.Test;

/**
 *
 * @author vladislav
 */
public class SkillValidatorTest extends TestBase {

  AttributeDomainMapper mapper = new AttributeDomainMapper();

  String routerRef = "router-ref";

  @Before
  public void init() throws CommsRouterException {

    routerService.replace(newCreateRouterArg("router-name", ""), routerRef);

    createBooleanSkill("isTechnical", routerRef);
    createNumericSkill("ageWithBoundary", 18, true, 45, false, 50, true, 60, false, Boolean.FALSE, routerRef);
    createMultiNumericSkill("multiAgeWithBoundary", 18, true, 45, true, Boolean.FALSE, routerRef);
    createNumericSkill("age", Boolean.FALSE, routerRef);
    createEnumerationSkill("language", new HashSet<String>(Arrays.asList("en", "fr", "es", "ru", "bg")), routerRef);
    createStringSkill("nameWithRegex", "[a-zA-Z]+", Boolean.TRUE, routerRef);
    createStringSkill("name", Boolean.TRUE, routerRef);

  }

  @Test
  public void testAttributeGroupDtoOK() throws CommsRouterException {
    AttributeGroupDto attributeGroupDto = new AttributeGroupDto();
    attributeGroupDto.add("isTechnical", true);
    attributeGroupDto.add("age", 5.0);
    attributeGroupDto.add("ageWithBoundary", 18.0);
    attributeGroupDto.add("multiAgeWithBoundary", 18.0);
    attributeGroupDto.add("language", "en");
    attributeGroupDto.add("nameWithRegex", "suzie");
    attributeGroupDto.add("name", "suzie5");
    skillValidator.validate(attributeGroupDto, routerRef);
  }

  @Test(expected = CommsRouterException.class)
  public void testDoubleUnderInterval() throws CommsRouterException {
    AttributeGroupDto attributeGroupDto = new AttributeGroupDto();
    attributeGroupDto.add("ageWithBoundary", 5.0);
    skillValidator.validate(attributeGroupDto, routerRef);
  }

  @Test
  public void testDoubleOnInclusiveBoundary() throws CommsRouterException {
    AttributeGroupDto attributeGroupDto = new AttributeGroupDto();
    attributeGroupDto.add("ageWithBoundary", 18.0);
    skillValidator.validate(attributeGroupDto, routerRef);
  }

  @Test(expected = CommsRouterException.class)
  public void testDoubleOnExclusiveBoundary() throws CommsRouterException {
    AttributeGroupDto attributeGroupDto = new AttributeGroupDto();
    attributeGroupDto.add("ageWithBoundary", 45.0);
    skillValidator.validate(attributeGroupDto, routerRef);
  }

  @Test(expected = CommsRouterException.class)
  public void testDoubleAboveInterval() throws CommsRouterException {
    AttributeGroupDto attributeGroupDto = new AttributeGroupDto();
    attributeGroupDto.add("ageWithBoundary", 46.0);
    skillValidator.validate(attributeGroupDto, routerRef);
  }

  @Test
  public void testDoubleInSecondInterval() throws CommsRouterException {
    AttributeGroupDto attributeGroupDto = new AttributeGroupDto();
    attributeGroupDto.add("ageWithBoundary", 55.0);
    skillValidator.validate(attributeGroupDto, routerRef);
  }

  @Test(expected = CommsRouterException.class)
  public void testSkillDoesNotExist() throws CommsRouterException {
    AttributeGroupDto attributeGroupDto = new AttributeGroupDto();
    attributeGroupDto.add("babaiaga", true);
    skillValidator.validate(attributeGroupDto, routerRef);
  }

  @Test(expected = CommsRouterException.class)
  public void testSkillUnexpectedValue() throws CommsRouterException {
    AttributeGroupDto attributeGroupDto = new AttributeGroupDto();
    attributeGroupDto.add("isTechnical", "yes");
    skillValidator.validate(attributeGroupDto, routerRef);
  }

  @Test(expected = CommsRouterException.class)
  public void testSkillBadStringValue() throws CommsRouterException {
    AttributeGroupDto attributeGroupDto = new AttributeGroupDto();
    attributeGroupDto.add("nameWithRegex", "suzie5");
    skillValidator.validate(attributeGroupDto, routerRef);
  }

  @Test(expected = CommsRouterException.class)
  public void testSkillBadEnumValue() throws CommsRouterException {
    AttributeGroupDto attributeGroupDto = new AttributeGroupDto();
    attributeGroupDto.add("language", "pl");
    skillValidator.validate(attributeGroupDto, routerRef);
  }

  @Test(expected = CommsRouterException.class)
  public void testSkillBadNumberValue() throws CommsRouterException {
    AttributeGroupDto attributeGroupDto = new AttributeGroupDto();
    attributeGroupDto.add("ageWithBoundary", "5");
    skillValidator.validate(attributeGroupDto, routerRef);
  }

  private void createBooleanSkill(String name, String routerRef) throws CommsRouterException {
    createSkill(name, new BoolAttributeDomainDto(), false, routerRef);
  }

  private void createNumericSkill(String name, double bLow, boolean inclusiveBLow, double bHigh, boolean inclusiveBHigh, double b1Low, boolean inclusiveB1Low, double b1High, boolean inclusiveB1High, boolean multivalue, String routerRef) throws CommsRouterException {
    NumberIntervalBoundary numberIntervalBoundary1 = new NumberIntervalBoundary(bLow, inclusiveBLow);
    NumberIntervalBoundary numberIntervalBoundary2 = new NumberIntervalBoundary(bHigh, inclusiveBHigh);
    NumberIntervalBoundary numberIntervalBoundary3 = new NumberIntervalBoundary(b1Low, inclusiveB1Low);
    NumberIntervalBoundary numberIntervalBoundary4 = new NumberIntervalBoundary(b1High, inclusiveB1High);
    NumberInterval interval1 = new NumberInterval(numberIntervalBoundary1, numberIntervalBoundary2);
    NumberInterval interval2 = new NumberInterval(numberIntervalBoundary3, numberIntervalBoundary4);
    AttributeDomainDto attributeDomainDto = new NumberAttributeDomainDto(Lists.newArrayList(interval1, interval2));
    createSkill(name, attributeDomainDto, multivalue, routerRef);
  }

  private void createMultiNumericSkill(String name, double b1, boolean inclusiveB1, double b2, boolean inclusiveB2, boolean multivalue, String routerRef) throws CommsRouterException {
    NumberIntervalBoundary numberIntervalBoundary1 = new NumberIntervalBoundary(b1, inclusiveB1);
    NumberIntervalBoundary numberIntervalBoundary2 = new NumberIntervalBoundary(b2, inclusiveB2);
    NumberInterval interval = new NumberInterval(numberIntervalBoundary1, numberIntervalBoundary2);
    NumberInterval interval1 = new NumberInterval(new NumberIntervalBoundary(0.0, false), new NumberIntervalBoundary(1.0, false));

    AttributeDomainDto attributeDomainDto = new NumberAttributeDomainDto(Lists.newArrayList(interval1, interval));
    createSkill(name, attributeDomainDto, multivalue, routerRef);
  }

  private void createNumericSkill(String name, boolean multivalue, String routerRef) throws CommsRouterException {
    AttributeDomainDto attributeDomainDto = new NumberAttributeDomainDto();
    createSkill(name, attributeDomainDto, multivalue, routerRef);
  }

  private void createEnumerationSkill(String name, Set<String> values, String routerRef) throws CommsRouterException {
    AttributeDomainDto attributeDomainDto = new EnumerationAttributeDomainDto(values);
    createSkill(name, attributeDomainDto, false, routerRef);
  }

  private void createStringSkill(String name, String regex, Boolean multivalue, String routerRef) throws CommsRouterException {
    AttributeDomainDto attributeDomainDto = new StringAttributeDomainDto(regex);
    createSkill(name, attributeDomainDto, multivalue, routerRef);
  }

  private void createStringSkill(String name, Boolean multivalue, String routerRef) throws CommsRouterException {
    AttributeDomainDto attributeDomainDto = new StringAttributeDomainDto();
    createSkill(name, attributeDomainDto, multivalue, routerRef);
  }

  private void createSkill(String name, AttributeDomainDto attributeDomainDto, Boolean multivalue, String routerRef) throws CommsRouterException {
    CreateSkillArg arg = new CreateSkillArg();
    arg.setDomain(attributeDomainDto);
    arg.setMultivalue(multivalue);
    RouterObjectRef ror = RouterObjectRef.builder().setRef(name).setRouterRef(routerRef).build();
    skillService.replace(arg, ror);
  }
}
