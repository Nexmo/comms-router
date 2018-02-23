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


import com.softavail.commsrouter.api.dto.model.skill.AttributeValueType;
import com.softavail.commsrouter.api.dto.model.skill.EnumerationAttributeValueDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberAttributeValueDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.AttributeValueDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.AttributeValueDomainDtoVisitor;
import com.softavail.commsrouter.api.dto.model.skill.NumberInterval;
import com.softavail.commsrouter.api.dto.model.skill.NumberIntervalBoundary;
import com.softavail.commsrouter.api.dto.model.skill.StringAttributeValueDomainDto;
import com.softavail.commsrouter.domain.AttributeValueDefinition;
import com.softavail.commsrouter.domain.AttributeValueDomain;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 *
 * @author ikrustev
 */
public class AttributeValueDomainMapper {

  public static AttributeValueType getAttributeValueType(AttributeValueDefinition jpa) {
    if (jpa.getEnumValue() != null) {
      assert jpa.getBoundary() == null && jpa.getInclusive() == null && jpa.getRegex() == null;
      return AttributeValueType.enumeration;
    }
    if (jpa.getBoundary() != null) {
      assert jpa.getEnumValue() == null && jpa.getRegex() == null;
      return AttributeValueType.number;
    }
    if (jpa.getRegex() != null) {
      assert jpa.getEnumValue() == null && jpa.getBoundary() == null && jpa.getInclusive() == null;
      return AttributeValueType.string;
    }
    throw new RuntimeException("AttributeValueDefinition with no value: " + jpa.getId());
  }

  public AttributeValueDomainDto toDto(AttributeValueDomain jpa) {
    if (jpa == null) {
      return null;
    }
    switch (jpa.getType()) {
      case enumeration:
        return toEnumerationDto(jpa);
      case number:
        return toNumberDto(jpa);
      case string:
        return toStringDto(jpa);
    }
    throw new RuntimeException("Unexpected attribute value domain type: " + jpa.getType());
  }

  private EnumerationAttributeValueDomainDto toEnumerationDto(AttributeValueDomain jpa) {
    Set<String> values = jpa.getValueDefinitions().stream()
            .map(def -> {
              assert getAttributeValueType(def) == jpa.getType();
              return def.getEnumValue();
            })
            .collect(Collectors.toSet());
    return new EnumerationAttributeValueDomainDto(values);
  }

  private NumberAttributeValueDomainDto toNumberDto(AttributeValueDomain jpa) {
    ArrayList<NumberInterval> intervals = jpa.getValueDefinitions().stream()
            .map(def -> {
              assert getAttributeValueType(def) == jpa.getType();
              return new NumberIntervalBoundary(def.getBoundary(), def.getInclusive());
            })
            .collect(Collector.of(
                    () -> new ArrayList<>(),
                    (result, boundary) -> {
                      NumberInterval incomplete = getLastIncompleteInterval(result);
                      if (incomplete == null) {
                        NumberInterval interval = new NumberInterval();
                        interval.setLow(boundary);
                        result.add(interval);
                      } else {
                        incomplete.setHigh(boundary);
                      }
                    },
                    (result1, result2) -> result1,
                    Collector.Characteristics.IDENTITY_FINISH)
            );
    assert getLastIncompleteInterval(intervals) == null;
    return new NumberAttributeValueDomainDto(intervals);
  }

  static private NumberInterval getLastIncompleteInterval(ArrayList<NumberInterval> list) {
    int size = list.size();
    if (size >= 1) {
      NumberInterval last = list.get(size - 1);
      if (last.getHigh() == null) {
        return last;
      }
    }
    return null;
  }

  private StringAttributeValueDomainDto toStringDto(AttributeValueDomain jpa) {
    assert jpa.getValueDefinitions().size() <= 1;
    String regEx = jpa.getValueDefinitions().stream()
            .findFirst()
            .map(def -> def.getRegex())
            .orElse(null);
    return new StringAttributeValueDomainDto(regEx);
  }

  public AttributeValueDomain fromDto(AttributeValueDomainDto dto) {
    if (dto == null) {
      return null;
    }
    AttributeValueDomain jpa = new AttributeValueDomain();
    jpa.setType(dto.getType());

    dto.accept(new AttributeValueDomainDtoVisitor() {

      @Override
      public void handleEnumerationValues(Set<String> values) {
        values.stream().forEach(value -> jpa.addEnumValue(value));
      }

      @Override
      public void handleNumberIntervals(List<NumberInterval> intervals) {
        intervals.forEach(interval -> {
          jpa.addIntervalBoundary(interval.getLow());
          jpa.addIntervalBoundary(interval.getHigh());
        });
      }

      @Override
      public void handleRegex(String regex) {
        jpa.addRegex(regex);
      }
    });
        
    return jpa;
  }

}
