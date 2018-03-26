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
import java.io.IOException;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author vladislav
 */
public class SkillValidator {

  private static final Logger LOGGER = LogManager.getLogger(SkillValidator.class);

  private final CoreSkillService coreSkillService;

  public SkillValidator(CoreSkillService coreSkillService) {
    this.coreSkillService = coreSkillService;
  }

  public void validate(AttributeGroupDto capabilities, String routerRef) throws CommsRouterException {
    try {
      for(Map.Entry<String, AttributeValueDto> capability : capabilities.entrySet()) {
        validateCapability(capability.getKey(), capability.getValue(), routerRef);
      }
    } catch (IOException ex) {
      throw new CommsRouterException(ex);
    }
  }

  private void validateCapability(String skill, AttributeValueDto value, String routerRef) throws IOException {
    SkillDto skillDto = validateSkillExistance(skill, routerRef);
    validateValuesNumber(skillDto, skill, value);
    validateValuesType(skillDto, skill, value);
    validateValuesRestrictions(skillDto, skill, value);
  }

  private SkillDto validateSkillExistance(String skill, String routerRef) throws IOException {
    try {
      return coreSkillService.get(new RouterObjectRef(skill, routerRef));
    } catch (NotFoundException ex) {
      throw new IOException("Skill " + skill + " was not found.", ex);
    } catch (CommsRouterException ex) {
      throw new IOException("Error while retrieving skill " + skill + " from database.", ex);
    }
  }

  private void validateValuesNumber(SkillDto skillDto, String skill, AttributeValueDto attributeValueDto)
          throws IOException {
    try {
      attributeValueDto.accept( new AttributeValueVisitor() {
        @Override
        public void handleStringValue(StringAttributeValueDto value) throws IOException {
          singlevalueValidation(value);
        }
        @Override
        public void handleDoubleValue(DoubleAttributeValueDto value) throws IOException {
          singlevalueValidation(value);
        }
        @Override
        public void handleBooleanValue(BooleanAttributeValueDto value) throws IOException {
          singlevalueValidation(value);
        }
        @Override
        public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value) throws IOException{
          multivalueValidation(value);
        }
        @Override
        public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value) throws IOException {
          multivalueValidation(value);
        }

        private void singlevalueValidation(AttributeValueDto value) throws IOException {
          if (skillDto.getMultivalue()) {
            throw new IOException("Skill " + skill + " should have a multivalue parameter: " + value.toString());
          }
        }
        private void multivalueValidation(AttributeValueDto value) throws IOException {
          if (!skillDto.getMultivalue()) {
              throw new IOException("Skill " + skill + " does not support multiple values: " + value.toString());
          }
        }
      });
    } catch (IOException ex) {
      LOGGER.error("Unexpected exception", ex);
    }
  }

  private void validateValuesType(SkillDto skillDto, String skill, AttributeValueDto attributeValueDto) throws IOException {
    try {
      attributeValueDto.accept( new AttributeValueVisitor() {
        @Override
        public void handleStringValue(StringAttributeValueDto value) throws IOException {
          if (skillDto.getDomain().getType() != AttributeType.string && skillDto.getDomain().getType() != AttributeType.enumeration) {
            throw new IOException("Invalid value for skill " + skill + ": " + value.toString());
          }
        }
        @Override
        public void handleDoubleValue(DoubleAttributeValueDto value) throws IOException {
          if (skillDto.getDomain().getType() != AttributeType.number) {
            throw new IOException("Invalid value for skill " + skill + ": " + value.toString());
          }
        }
        @Override
        public void handleBooleanValue(BooleanAttributeValueDto value) throws IOException {
          if (skillDto.getDomain().getType() != AttributeType.bool) {
            throw new IOException("Invalid value for skill " + skill + ": " + value.toString());
          }
        }
        @Override
        public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value) throws IOException{
          if (skillDto.getDomain().getType() != AttributeType.string && skillDto.getDomain().getType() != AttributeType.enumeration) {
            throw new IOException("Invalid value for skill " + skill + ": " + value.toString());
          }
        }
        @Override
        public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value) throws IOException {
          if (skillDto.getDomain().getType() != AttributeType.number) {
            throw new IOException("Invalid value for skill " + skill + ": " + value.toString());
          }
        }
      });
    } catch (IOException ex) {
      LOGGER.error("Unexpected exception", ex);
    }
  }
  private void validateValuesRestrictions(SkillDto skillDto, String skill, AttributeValueDto attributeValueDto) throws IOException {
    attributeValueDto.accept( new AttributeValueVisitor() {
      @Override
      public void handleStringValue(StringAttributeValueDto value) throws IOException {
        switch (skillDto.getDomain().getType()) {
          case string:
            String regExp = ((StringAttributeDomainDto) skillDto.getDomain()).getRegex();
            if (!value.getValue().matches(regExp)) {
              throw new IOException("Invalid value for skill " + skill + ": " + value.getValue());
            }
            break;
          case enumeration:
            if (!((EnumerationAttributeDomainDto) skillDto.getDomain()).getValues().contains(value.getValue())) {
              throw new IOException("Invalid value for skill " + skill + ": " + value.getValue());
            }
            break;
          default:
            throw new IOException("Unexpected skill type: " + skillDto.getDomain().getType());
        }
      }
      @Override
      public void handleDoubleValue(DoubleAttributeValueDto value) throws IOException {
      }
      @Override
      public void handleBooleanValue(BooleanAttributeValueDto value) throws IOException {
      }
      @Override
      public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value) throws IOException{
        switch (skillDto.getDomain().getType()) {
          case string:
            String regExp = ((StringAttributeDomainDto) skillDto.getDomain()).getRegex();
            for (String v : value.getValue()) {
              if (!v.matches(regExp)) {
                throw new IOException("Invalid value for skill " + skill + ": " + v);
              }
            }
            break;
          case enumeration:
            for (String v : value.getValue()) {
              if (!((EnumerationAttributeDomainDto) skillDto.getDomain()).getValues().contains(v)) {
                throw new IOException("Invalid value for skill " + skill + ": " + v);
              }
            }
            break;
          default:
            throw new IOException("Unexpected skill type: " + skillDto.getDomain().getType());
        }
      }
      @Override
      public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value) throws IOException {

      }
    });
  }

}
