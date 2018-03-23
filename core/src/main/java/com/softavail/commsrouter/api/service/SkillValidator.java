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
import com.softavail.commsrouter.api.dto.model.skill.SkillDto;
import com.softavail.commsrouter.api.dto.model.skill.StringAttributeDomainDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.eval.RsqlSkillsValidator;
import java.util.Map;
import javax.inject.Inject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author vladislav
 */
class SkillValidator {

  private static final Logger LOGGER = LogManager.getLogger(SkillValidator.class);

  @Inject
  private CoreSkillService coreSkillService;

  @Inject
  private RsqlSkillsValidator rsqlSkillsValidator;

  public void validateCapabilities(AttributeGroupDto capabilities, String routerRef) throws CommsRouterException {
    for(Map.Entry<String, AttributeValueDto> capability : capabilities.entrySet()) {
      validateCapability(capability.getKey(), capability.getValue(), routerRef);
    }
  }

  private void validateCapability(String skill, AttributeValueDto value, String routerRef) throws CommsRouterException {
    SkillDto skillDto = validateSkillExistance(skill, routerRef);
    validateValuesNumber(skillDto, skill, value);
    validateValuesType(skillDto, skill, value);
    validateValuesRestrictions(skillDto, skill, value);
  }

  private SkillDto validateSkillExistance(String skill, String routerRef) throws CommsRouterException {
    try {
      return coreSkillService.get(new RouterObjectRef(skill, routerRef));
    } catch (NotFoundException ex) {
      throw new CommsRouterException("Skill " + skill + " was not found.", ex);
    } catch (CommsRouterException ex) {
      throw new CommsRouterException("Error while retrieving skill " + skill + " from database.", ex);
    }
  }

  private void validateValuesNumber(SkillDto skillDto, String skill, AttributeValueDto attributeValueDto)
          throws CommsRouterException {
    
    attributeValueDto.accept( new AttributeValueVisitor() {
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
      public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value) throws CommsRouterException{
        multivalueValidation(value);
      }
      @Override
      public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value) throws CommsRouterException {
        multivalueValidation(value);
      }

      private void singlevalueValidation(AttributeValueDto value) throws CommsRouterException {
        if (skillDto.getMultivalue()) {
          throw new CommsRouterException("Skill " + skill + " should have a multivalue parameter: " + value.toString());
        }
      }
      private void multivalueValidation(AttributeValueDto value) throws CommsRouterException {
        if (!skillDto.getMultivalue()) {
            throw new CommsRouterException("Skill " + skill + " does not support multiple values: " + value.toString());
        }
      }
    });
  }

  private void validateValuesType(SkillDto skillDto, String skill, AttributeValueDto attributeValueDto) throws CommsRouterException {
    try {
      attributeValueDto.accept( new AttributeValueVisitor() {
        @Override
        public void handleStringValue(StringAttributeValueDto value) throws CommsRouterException {
          if (skillDto.getDomain().getType() != AttributeType.string && skillDto.getDomain().getType() != AttributeType.enumeration) {
            throw new CommsRouterException("Invalid value for skill " + skill + ": " + value.toString());
          }
        }
        @Override
        public void handleDoubleValue(DoubleAttributeValueDto value) throws CommsRouterException {
          if (skillDto.getDomain().getType() != AttributeType.number) {
            throw new CommsRouterException("Invalid value for skill " + skill + ": " + value.toString());
          }
        }
        @Override
        public void handleBooleanValue(BooleanAttributeValueDto value) throws CommsRouterException {
          if (skillDto.getDomain().getType() != AttributeType.bool) {
            throw new CommsRouterException("Invalid value for skill " + skill + ": " + value.toString());
          }
        }
        @Override
        public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value) throws CommsRouterException{
          if (skillDto.getDomain().getType() != AttributeType.string && skillDto.getDomain().getType() != AttributeType.enumeration) {
            throw new CommsRouterException("Invalid value for skill " + skill + ": " + value.toString());
          }
        }
        @Override
        public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value) throws CommsRouterException {
          if (skillDto.getDomain().getType() != AttributeType.number) {
            throw new CommsRouterException("Invalid value for skill " + skill + ": " + value.toString());
          }
        }
      });
    } catch (CommsRouterException ex) {
      LOGGER.error("Unexpected exception", ex);
    }
  }
  private void validateValuesRestrictions(SkillDto skillDto, String skill, AttributeValueDto attributeValueDto) throws CommsRouterException {
    try {
      attributeValueDto.accept( new AttributeValueVisitor() {
        @Override
        public void handleStringValue(StringAttributeValueDto value) throws CommsRouterException {
          switch (skillDto.getDomain().getType()) {
            case string:
              String regExp = ((StringAttributeDomainDto) skillDto.getDomain()).getRegex();
              if (!value.getValue().matches(regExp)) {
                throw new CommsRouterException("Invalid value for skill " + skill + ": " + value.getValue());
              }
              break;
            case enumeration:
              if (!((EnumerationAttributeDomainDto) skillDto.getDomain()).getValues().contains(value.getValue())) {
                throw new CommsRouterException("Invalid value for skill " + skill + ": " + value.getValue());
              }
              break;
            default:
              throw new CommsRouterException("Unexpected skill type: " + skillDto.getDomain().getType());
          }
        }
        @Override
        public void handleDoubleValue(DoubleAttributeValueDto value) throws CommsRouterException {
        }
        @Override
        public void handleBooleanValue(BooleanAttributeValueDto value) throws CommsRouterException {
        }
        @Override
        public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value) throws CommsRouterException{
          switch (skillDto.getDomain().getType()) {
            case string:
              String regExp = ((StringAttributeDomainDto) skillDto.getDomain()).getRegex();
              for (String v : value.getValue()) {
                if (!v.matches(regExp)) {
                  throw new CommsRouterException("Invalid value for skill " + skill + ": " + v);
                }
              }
              break;
            case enumeration:
              for (String v : value.getValue()) {
                if (!((EnumerationAttributeDomainDto) skillDto.getDomain()).getValues().contains(v)) {
                  throw new CommsRouterException("Invalid value for skill " + skill + ": " + v);
                }
              }
              break;
            default:
              throw new CommsRouterException("Unexpected skill type: " + skillDto.getDomain().getType());
          }
        }
        @Override
        public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value) throws CommsRouterException {

        }
      });
    } catch (CommsRouterException ex) {
      LOGGER.error("Unexpected exception", ex);
    }
  }

}
