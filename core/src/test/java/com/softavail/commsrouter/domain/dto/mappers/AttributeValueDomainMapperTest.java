/*
 * Copyright 2018 ikrustev.
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

import com.softavail.commsrouter.api.dto.model.skill.AttributeValueDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.AttributeValueType;
import com.softavail.commsrouter.api.dto.model.skill.NumberAttributeValueDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberInterval;
import com.softavail.commsrouter.api.dto.model.skill.NumberIntervalBoundary;
import com.softavail.commsrouter.domain.AttributeValueDefinition;
import java.util.Arrays;
import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author ikrustev
 */
public class AttributeValueDomainMapperTest {

  private final AttributeValueDomainMapper mapper;

  public AttributeValueDomainMapperTest() {
    this.mapper = new AttributeValueDomainMapper();
  }

  @Test
  public void testGetAttributeValueType() {
    AttributeValueType actual;
    AttributeValueDefinition def;

    def = new AttributeValueDefinition();
    def.setEnumValue("enum-value");
    actual = AttributeValueDomainMapper.getAttributeValueType(def);
    assertEquals(AttributeValueType.enumeration, actual);

    def = new AttributeValueDefinition();
    def.setBoundary(Double.POSITIVE_INFINITY);
    actual = AttributeValueDomainMapper.getAttributeValueType(def);
    assertEquals(AttributeValueType.number, actual);

    def = new AttributeValueDefinition();
    def.setRegex("regex-value");
    actual = AttributeValueDomainMapper.getAttributeValueType(def);
    assertEquals(AttributeValueType.string, actual);
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

    NumberAttributeValueDomainDto expected = new NumberAttributeValueDomainDto(intevals);
    AttributeValueDomainDto actual = mapper.toDto(mapper.fromDto(expected));
    assertEquals(expected, actual);
  }

}
