/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.domain.dto.mappers;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;

import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfBooleansAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfDoublesAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.ArrayOfStringsAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeValueVisitor;
import com.softavail.commsrouter.api.dto.model.attribute.BooleanAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.DoubleAttributeValueDto;
import com.softavail.commsrouter.api.dto.model.attribute.StringAttributeValueDto;
import com.softavail.commsrouter.domain.Attribute;
import com.softavail.commsrouter.domain.AttributeGroup;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.util.Iterator;
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

  public static JpaAttributeValueType getJpaAttributeValueType(Attribute jpa) {
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

    ListMultimap<String, AttributeValueDto> attributesMap = ArrayListMultimap.create();

    jpa.getAttributes().stream().forEach(jpaAttribute -> {
      String name = jpaAttribute.getName();
      JpaAttributeValueType valueType = getJpaAttributeValueType(jpaAttribute);
      switch (valueType) {
        case STRING:
          attributesMap.put(name, new StringAttributeValueDto(jpaAttribute.getStringValue()));
          break;
        case DOUBLE:
          attributesMap.put(name, new DoubleAttributeValueDto(jpaAttribute.getDoubleValue()));
          break;
        case BOOLEAN:
          attributesMap.put(name, new BooleanAttributeValueDto(jpaAttribute.getBooleanValue()));
          break;
        default:
          throw new RuntimeException("Unexpected attribute value type " + valueType + " for " + name
              + "in " + jpa.getId());
      }
    });

    AttributeGroupDto dto = new AttributeGroupDto();
    attributesMap.asMap().forEach((key, value) -> {
      Iterator<AttributeValueDto> iterator = value.iterator();
      AttributeValueDto firstValue = iterator.next();
      if (value.size() == 1) {
        dto.put(key, firstValue);
      } else {
        // value collections in this multimap view are always non-empty, so here we have more than
        // one element
        try {
          firstValue.accept(new AttributeValueVisitor() {
            @Override
            public void handleBooleanValue(BooleanAttributeValueDto value) throws IOException {
              throw new RuntimeException(
                  "Unexpected boolean array value for " + key + " in " + jpa.getId());
            }

            @Override
            public void handleDoubleValue(DoubleAttributeValueDto value) throws IOException {
              ArrayOfDoublesAttributeValueDto arrayValue;
              arrayValue = new ArrayOfDoublesAttributeValueDto();
              arrayValue.add(value.getValue());
              while (iterator.hasNext()) {
                iterator.next().accept(new AttributeValueVisitor() {
                  @Override
                  public void handleBooleanValue(BooleanAttributeValueDto value)
                      throws IOException {
                    throw new RuntimeException(
                        "Mixed array boolean -> long for " + key + " in " + jpa.getId());
                  }

                  @Override
                  public void handleDoubleValue(DoubleAttributeValueDto value)
                      throws IOException {
                    arrayValue.add(value.getValue());
                  }

                  @Override
                  public void handleStringValue(StringAttributeValueDto value) throws IOException {
                    throw new RuntimeException(
                        "Mixed array string -> long for " + key + " in " + jpa.getId());
                  }

                  @Override
                  public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value)
                      throws IOException {
                    throw new RuntimeException(
                        "Nested array -> long for " + key + " in " + jpa.getId());
                  }

                  @Override
                  public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
                      throws IOException {
                    throw new RuntimeException(
                        "Nested array -> long for " + key + " in " + jpa.getId());
                  }

                  @Override
                  public void handleArrayOfBooleansValue(ArrayOfBooleansAttributeValueDto value)
                      throws IOException {
                    throw new RuntimeException(
                        "Nested array -> long for " + key + " in " + jpa.getId());
                  }
                });
              }
              dto.put(key, arrayValue);
            }

            @Override
            public void handleStringValue(StringAttributeValueDto value) throws IOException {
              ArrayOfStringsAttributeValueDto arrayValue;
              arrayValue = new ArrayOfStringsAttributeValueDto();
              arrayValue.add(value.getValue());
              while (iterator.hasNext()) {
                iterator.next().accept(new AttributeValueVisitor() {
                  @Override
                  public void handleBooleanValue(BooleanAttributeValueDto value)
                      throws IOException {
                    throw new RuntimeException(
                        "Mixed array boolean -> string for " + key + " in " + jpa.getId());
                  }

                  @Override
                  public void handleDoubleValue(DoubleAttributeValueDto value) throws IOException {
                    throw new RuntimeException(
                        "Mixed array long -> string for " + key + " in " + jpa.getId());
                  }

                  @Override
                  public void handleStringValue(StringAttributeValueDto value) throws IOException {
                    arrayValue.add(value.getValue());
                  }

                  @Override
                  public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value)
                      throws IOException {
                    throw new RuntimeException(
                        "Nested array -> string for " + key + " in " + jpa.getId());
                  }

                  @Override
                  public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
                      throws IOException {
                    throw new RuntimeException(
                        "Nested array -> string for " + key + " in " + jpa.getId());
                  }

                  @Override
                  public void handleArrayOfBooleansValue(ArrayOfBooleansAttributeValueDto value)
                      throws IOException {
                    throw new RuntimeException(
                        "Nested array -> string for " + key + " in " + jpa.getId());
                  }
                });
              }
              dto.put(key, arrayValue);
            }

            @Override
            public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value)
                throws IOException {
              throw new RuntimeException(
                  "Unexpected array array value for " + key + " in " + jpa.getId());
            }

            @Override
            public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
                throws IOException {
              throw new RuntimeException(
                  "Unexpected array array value for " + key + " in " + jpa.getId());
            }

            @Override
            public void handleArrayOfBooleansValue(ArrayOfBooleansAttributeValueDto value)
                throws IOException {
              throw new RuntimeException(
                  "Unexpected array array value for " + key + " in " + jpa.getId());
            }
          });
        } catch (IOException ex) {
          throw new RuntimeException("Unexpected exception while processing attibute values", ex);
        }
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
              jpa.add(key, element);
            });
          }

          @Override
          public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value)
              throws IOException {
            List<Double> elements = value.getValue();
            elements.forEach(element -> {
              jpa.add(key, element);
            });
          }

          @Override
          public void handleArrayOfBooleansValue(ArrayOfBooleansAttributeValueDto value)
              throws IOException {
            List<Boolean> elements = value.getValue();
            elements.forEach(element -> {
              jpa.add(key, element);
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
