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
import com.softavail.commsrouter.api.dto.model.skill.AttributeDomainDtoVisitor;
import com.softavail.commsrouter.api.dto.model.skill.AttributeType;
import com.softavail.commsrouter.api.dto.model.skill.BoolAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.EnumerationAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberInterval;
import com.softavail.commsrouter.api.dto.model.skill.NumberIntervalBoundary;
import com.softavail.commsrouter.api.dto.model.skill.StringAttributeDomainDto;
import com.softavail.commsrouter.domain.AttributeDomain;
import com.softavail.commsrouter.domain.AttributeDomainDefinition;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;


/**
 *
 * @author ikrustev
 */
public class AttributeDomainMapper {

  public static AttributeType getAttributeType(AttributeDomainDefinition jpa) {
    if (jpa.getEnumValue() != null) {
      assert jpa.getBoundary() == null && jpa.getInclusive() == null && jpa.getRegex() == null;
      return AttributeType.enumeration;
    }
    if (jpa.getBoundary() != null) {
      assert jpa.getEnumValue() == null && jpa.getRegex() == null;
      return AttributeType.number;
    }
    if (jpa.getRegex() != null) {
      assert jpa.getEnumValue() == null && jpa.getBoundary() == null && jpa.getInclusive() == null;
      return AttributeType.string;
    }
    throw new RuntimeException("AttributeDomainDefinition with no value: " + jpa.getId());
  }

  public AttributeDomainDto toDto(AttributeDomain jpa) {
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
      case bool:
        return toBoolDto(jpa);
      default:
        throw new RuntimeException("Unexpected attribute type: " + jpa.getType());
    }
  }

  private EnumerationAttributeDomainDto toEnumerationDto(AttributeDomain jpa) {
    Set<String> values = jpa.getDefinitions().stream()
            .map(def -> {
              assert getAttributeType(def) == jpa.getType();
              return def.getEnumValue();
            })
            .collect(Collectors.toSet());
    return new EnumerationAttributeDomainDto(values);
  }

  private NumberAttributeDomainDto toNumberDto(AttributeDomain jpa) {
    ArrayList<NumberInterval> intervals = new ArrayList<>();
    jpa.getDefinitions().stream().forEachOrdered(
        def -> {
          NumberIntervalBoundary boundary =
                  new NumberIntervalBoundary(def.getBoundary(), def.getInclusive());
          NumberInterval incomplete = getLastIncompleteInterval(intervals);
          if (incomplete == null) {
            NumberInterval interval = new NumberInterval();
            interval.setLow(boundary);
            intervals.add(interval);
          } else {
            incomplete.setHigh(boundary);
          }
        }
    );
    assert getLastIncompleteInterval(intervals) == null;
    return new NumberAttributeDomainDto(intervals);
  }

  private static NumberInterval getLastIncompleteInterval(ArrayList<NumberInterval> list) {
    int size = list.size();
    if (size >= 1) {
      NumberInterval last = list.get(size - 1);
      if (last.getHigh() == null) {
        return last;
      }
    }
    return null;
  }

  private StringAttributeDomainDto toStringDto(AttributeDomain jpa) {
    assert jpa.getDefinitions().size() <= 1;
    String regEx = jpa.getDefinitions().stream()
            .findFirst()
            .map(def -> def.getRegex())
            .orElse(null);
    return new StringAttributeDomainDto(regEx);
  }

  private AttributeDomainDto toBoolDto(AttributeDomain jpa) {
    assert jpa.getDefinitions().isEmpty();
    return new BoolAttributeDomainDto();
  }

  public AttributeDomain fromDto(AttributeDomainDto dto) {
    if (dto == null) {
      return null;
    }
    AttributeDomain jpa = new AttributeDomain();
    jpa.setType(dto.getType());

    dto.accept(new AttributeDomainDtoVisitor() {

      @Override
      public void handleEnumerationValues(Set<String> values) {
        values.stream().forEach(value -> jpa.addEnumValue(value));
      }

      @Override
      public void handleNumberIntervals(List<NumberInterval> intervals) {
        if (intervals != null) {
          intervals.forEach(interval -> {
            jpa.addIntervalBoundary(interval.getLow());
            jpa.addIntervalBoundary(interval.getHigh());
          });
        }
      }

      @Override
      public void handleRegex(String regex) {
        jpa.addRegex(regex);
      }
    });
        
    return jpa;
  }

}
