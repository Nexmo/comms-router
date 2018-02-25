/*
 * Copyright 2017 - 2018 SoftAvail Inc.
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
package com.softavail.commsrouter.domain.dto.mappers;

import com.softavail.commsrouter.api.dto.model.skill.AttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.AttributeType;
import com.softavail.commsrouter.api.dto.model.skill.NumberAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberInterval;
import com.softavail.commsrouter.api.dto.model.skill.NumberIntervalBoundary;
import com.softavail.commsrouter.domain.AttributeDomainDefinition;
import java.util.Arrays;
import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author ikrustev
 */
public class AttributeDomainMapperTest {

  private final AttributeDomainMapper mapper;

  public AttributeDomainMapperTest() {
    this.mapper = new AttributeDomainMapper();
  }

  @Test
  public void testGetAttributeType() {
    AttributeType actual;
    AttributeDomainDefinition def;

    def = new AttributeDomainDefinition();
    def.setEnumValue("enum-value");
    actual = AttributeDomainMapper.getAttributeType(def);
    assertEquals(AttributeType.enumeration, actual);

    def = new AttributeDomainDefinition();
    def.setBoundary(Double.POSITIVE_INFINITY);
    actual = AttributeDomainMapper.getAttributeType(def);
    assertEquals(AttributeType.number, actual);

    def = new AttributeDomainDefinition();
    def.setRegex("regex-value");
    actual = AttributeDomainMapper.getAttributeType(def);
    assertEquals(AttributeType.string, actual);
  }

  @Test
  public void testFromDtoAndBackToDtoNumber() {
    List<NumberInterval> intevals = Arrays.asList(
        new NumberInterval(
            NumberIntervalBoundary.NEGATIVE_INFINITY,
            new NumberIntervalBoundary(-2.0)
        ),
        new NumberInterval(
            new NumberIntervalBoundary(-1.0),
            new NumberIntervalBoundary(1.0, true)
        ),
        new NumberInterval(
            new NumberIntervalBoundary(2.0, true),
            new NumberIntervalBoundary(3.0)
        ),
        new NumberInterval(
            new NumberIntervalBoundary(4.0, true),
            new NumberIntervalBoundary(5.0, true)
        ),
        new NumberInterval(
            new NumberIntervalBoundary(6.0, true),
            NumberIntervalBoundary.POSITIVE_INFINITY
        )
    );

    NumberAttributeDomainDto expected = new NumberAttributeDomainDto(intevals);
    AttributeDomainDto actual = mapper.toDto(mapper.fromDto(expected));
    assertEquals(expected, actual);
  }

}
