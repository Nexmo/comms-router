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

import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfDoublesAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfStringsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueVisitor;
import com.softavail.commsrouter.api.dto.model.attribute.BooleanAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.skill.AttributeType;
import com.softavail.commsrouter.api.dto.model.skill.EnumerationAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.NumberInterval;
import com.softavail.commsrouter.api.dto.model.skill.SkillDto;
import com.softavail.commsrouter.api.dto.model.skill.StringAttributeDomainDto;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.Map;


/**
 * Validator
 *
 * @author vladislav
 */
public class SkillValidator {

  private static final Logger LOGGER = LogManager.getLogger(SkillValidator.class);

  private final CoreSkillService coreSkillService;

  public SkillValidator(CoreSkillService coreSkillService) {
    this.coreSkillService = coreSkillService;
  }

  public void validate(AttributeGroupDto capabilities, String routerRef)
      throws CommsRouterException {
    if (capabilities != null) {
      for (Map.Entry<String, AttributeValueDto> capability : capabilities.entrySet()) {
        validateCapability(capability.getKey(), capability.getValue(), routerRef);
      }
    }
  }

  private void validateCapability(String skill, AttributeValueDto value, String routerRef)
      throws CommsRouterException {
    SkillDto skillDto = validateSkillExistance(skill, routerRef);
    validateValuesCount(skillDto, skill, value);
    validateValuesType(skillDto, skill, value);
    validateValuesRestrictions(skillDto, skill, value);
  }

  private SkillDto validateSkillExistance(String skill, String routerRef)
      throws CommsRouterException {
    try {
      return coreSkillService.get(new RouterObjectRef(skill, routerRef));
    } catch (NotFoundException ex) {
      throw new BadValueException("Skill " + skill + " was not found.", ex);
    }
  }

  private void validateValuesCount(SkillDto skillDto, String skill,
      AttributeValueDto attributeValueDto) throws CommsRouterException {

    attributeValueDto.accept(new AttributeValueVisitor() {
      @Override
      public void handleStringValue(StringAttributeValueDto value) throws CommsRouterException {
        singlevalueValidation(value);
      }

      @Override
      public void handleDoubleValue(DoubleAttributeValueDto value) throws CommsRouterException {
        singlevalueValidation(value);
      }

      @Override
      public void handleBooleanValue(BooleanAttributeValueDto value) throws CommsRouterException {
        singlevalueValidation(value);
      }

      @Override
      public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value)
          throws CommsRouterException {
        multivalueValidation(value);
      }

      @Override
      public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
          throws CommsRouterException {
        multivalueValidation(value);
      }

      private void singlevalueValidation(AttributeValueDto value) throws CommsRouterException {}

      private void multivalueValidation(AttributeValueDto value) throws CommsRouterException {
        if (!skillDto.getMultivalue()) {
          throw new BadValueException(
              "Skill " + skill + " does not support multiple values: " + value.toString());
        }
      }
    });
  }

  private void validateValuesType(SkillDto skillDto, String skill,
      AttributeValueDto attributeValueDto) throws CommsRouterException {
    attributeValueDto.accept(new AttributeValueVisitor() {
      @Override
      public void handleStringValue(StringAttributeValueDto value) throws CommsRouterException {
        if (skillDto.getDomain().getType() != AttributeType.string
            && skillDto.getDomain().getType() != AttributeType.enumeration) {
          throw new BadValueException(
              "Invalid value for skill " + skill + ": " + value.toString());
        }
      }

      @Override
      public void handleDoubleValue(DoubleAttributeValueDto value) throws CommsRouterException {
        if (skillDto.getDomain().getType() != AttributeType.number) {
          throw new BadValueException(
              "Invalid value for skill " + skill + ": " + value.toString());
        }
      }

      @Override
      public void handleBooleanValue(BooleanAttributeValueDto value) throws CommsRouterException {
        if (skillDto.getDomain().getType() != AttributeType.bool) {
          throw new BadValueException(
              "Invalid value for skill " + skill + ": " + value.toString());
        }
      }

      @Override
      public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value)
          throws CommsRouterException {
        if (skillDto.getDomain().getType() != AttributeType.string
            && skillDto.getDomain().getType() != AttributeType.enumeration) {
          throw new BadValueException(
              "Invalid value for skill " + skill + ": " + value.toString());
        }
      }

      @Override
      public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
          throws CommsRouterException {
        if (skillDto.getDomain().getType() != AttributeType.number) {
          throw new BadValueException(
              "Invalid value for skill " + skill + ": " + value.toString());
        }
      }
    });
  }

  private void validateValuesRestrictions(SkillDto skillDto, String skill,
      AttributeValueDto attributeValueDto) throws CommsRouterException {
    attributeValueDto.accept(new AttributeValueVisitor() {
      @Override
      public void handleStringValue(StringAttributeValueDto value) throws CommsRouterException {
        switch (skillDto.getDomain().getType()) {
          case string:
            String regExp = ((StringAttributeDomainDto) skillDto.getDomain()).getRegex();
            if (regExp != null && !value.getValue().matches(regExp)) {
              throw new BadValueException(
                  "Invalid value for skill " + skill + ": " + value.getValue());
            }
            break;
          case enumeration:
            if (!((EnumerationAttributeDomainDto) skillDto.getDomain()).getValues()
                .contains(value.getValue())) {
              throw new BadValueException(
                  "Invalid value for skill " + skill + ": " + value.getValue());
            }
            break;
          default:
            throw new BadValueException(
                "Unexpected skill type: " + skillDto.getDomain().getType());
        }
      }

      @Override
      public void handleDoubleValue(DoubleAttributeValueDto value) throws CommsRouterException {
        validateDoubleValue(value.getValue());
      }

      @Override
      public void handleBooleanValue(BooleanAttributeValueDto value) throws CommsRouterException {}

      @Override
      public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value)
          throws CommsRouterException {
        switch (skillDto.getDomain().getType()) {
          case string:
            String regExp = ((StringAttributeDomainDto) skillDto.getDomain()).getRegex();
            for (String v : value.getValue()) {
              if (regExp != null && !v.matches(regExp)) {
                throw new BadValueException("Invalid value for skill " + skill + ": " + v);
              }
            }
            break;
          case enumeration:
            for (String v : value.getValue()) {
              if (!((EnumerationAttributeDomainDto) skillDto.getDomain()).getValues().contains(v)) {
                throw new BadValueException("Invalid value for skill " + skill + ": " + v);
              }
            }
            break;
          default:
            throw new CommsRouterException(
                "Unexpected skill type: " + skillDto.getDomain().getType());
        }
      }

      @Override
      public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
          throws CommsRouterException {
        for (Double v : value.getValue()) {
          validateDoubleValue(v);
        }
      }

      private void validateDoubleValue(Double value) throws CommsRouterException {
        List<NumberInterval> intervals =
            ((NumberAttributeDomainDto) skillDto.getDomain()).getIntervals();
        if (intervals != null && !intervals.isEmpty()) {
          for (NumberInterval interval : intervals) {
            if (interval.contains(value)) {
              return;
            }
          }

          throw new BadValueException(
            "Invalid value for skill " + skill + ": " + value
            + ". Accepted intervals are " + intervals);
        }
      }
    });
  }

}
