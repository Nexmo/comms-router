/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;


import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfBooleansAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfDoublesAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfStringsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueVisitor;
import com.softavail.commsrouter.api.dto.model.attribute.BooleanAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.domain.Attribute;
import com.softavail.commsrouter.domain.AttributeGroup;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.util.List;

/**
 *
 * @author ikrustev
 */
public class AttributesMapper {

  private static final Logger LOGGER = LogManager.getLogger(AttributesMapper.class);

  public static enum JpaAttributeValueType {
    STRING, DOUBLE, BOOLEAN
  }

  private JpaAttributeValueType getJpaAttributeValueType(Attribute jpa) {
    if (jpa.getStringValue() != null) {
      assert jpa.getBooleanValue() == null && jpa.getDoubleValue() == null;
      return JpaAttributeValueType.STRING;
    }
    if (jpa.getDoubleValue() != null) {
      assert jpa.getBooleanValue() == null && jpa.getStringValue() == null;
      return JpaAttributeValueType.DOUBLE;
    }
    if (jpa.getBooleanValue() != null) {
      assert jpa.getStringValue() == null && jpa.getDoubleValue() == null;
      return JpaAttributeValueType.BOOLEAN;
    }
    throw new RuntimeException("Attribute with no value: " + jpa.getId() + " / " + jpa.getName());
  }

  public AttributeGroupDto toDto(AttributeGroup jpa) {
    if (jpa == null) {
      return null;
    }

    AttributeGroupDto dto = new AttributeGroupDto();
    jpa.getAttributes().stream().forEach(jpaAttribute -> {
      String name = jpaAttribute.getName();
      JpaAttributeValueType valueType = getJpaAttributeValueType(jpaAttribute);
      switch (valueType) {
        case STRING:
          if (jpaAttribute.isScalar()) {
            dto.add(name, jpaAttribute.getStringValue());
          } else {
            dto.addToArray(name, jpaAttribute.getStringValue());
          }
          break;
        case DOUBLE:
          if (jpaAttribute.isScalar()) {
            dto.add(name, jpaAttribute.getDoubleValue());
          } else {
            dto.addToArray(name, jpaAttribute.getDoubleValue());
          }
          break;
        case BOOLEAN:
          if (jpaAttribute.isScalar()) {
            dto.add(name, jpaAttribute.getBooleanValue());
          } else {
            dto.addToArray(name, jpaAttribute.getBooleanValue());
          }
          break;
        default:
          throw new RuntimeException("Unexpected attribute value type " + valueType + " for " + name
              + "in " + jpa.getId());
      }
    });

    return dto;
  }

  public AttributeGroup fromDto(AttributeGroupDto dto) {
    if (dto == null) {
      return null;
    }
    AttributeGroup jpa = new AttributeGroup();
    dto.forEach((key, value) -> {
      try {
        if (value == null) {
          throw new RuntimeException("Unexpected NULL value for \"" + key + "\"");
        }
        value.accept(new AttributeValueVisitor() {
          @Override
          public void handleBooleanValue(BooleanAttributeValueDto value) throws IOException {
            jpa.add(key, value.getValue());
          }

          @Override
          public void handleDoubleValue(DoubleAttributeValueDto value) throws IOException {
            jpa.add(key, value.getValue());
          }

          @Override
          public void handleStringValue(StringAttributeValueDto value) throws IOException {
            jpa.add(key, value.getValue());
          }

          @Override
          public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value)
              throws IOException {
            List<String> elements = value.getValue();
            elements.forEach(element -> {
              jpa.addArrayItem(key, element);
            });
          }

          @Override
          public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
              throws IOException {
            List<Double> elements = value.getValue();
            elements.forEach(element -> {
              jpa.addArrayItem(key, element);
            });
          }

          @Override
          public void handleArrayOfBooleansValue(ArrayOfBooleansAttributeValueDto value)
              throws IOException {
            List<Boolean> elements = value.getValue();
            elements.forEach(element -> {
              jpa.addArrayItem(key, element);
            });
          }
        });
      } catch (IOException ex) {
        LOGGER.error(ex.getLocalizedMessage());
      }
    });

    return jpa;
  }

}
