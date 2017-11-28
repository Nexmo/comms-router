/*
 * Copyright 2017 SoftAvail Inc.
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

package com.softavail.commsrouter.api.dto.model.attribute;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.util.Iterator;

/**
 * @author Ergyun Syuleyman
 */
public class AttributeValueDeserializer extends StdDeserializer<AttributeValueDto> {

  private static final Logger LOGGER = LogManager.getLogger(AttributeValueDeserializer.class);

  public AttributeValueDeserializer() {
    this(null);
  }

  public AttributeValueDeserializer(Class<?> vc) {
    super(vc);
  }

  @Override
  public AttributeValueDto deserialize(JsonParser jp, DeserializationContext ctxt)
      throws IOException, JsonProcessingException {
    JsonToken currentToken = jp.getCurrentToken();
    if (currentToken != null) {
      switch (currentToken) {
        case VALUE_NUMBER_INT:
        case VALUE_NUMBER_FLOAT:
          return new DoubleAttributeValueDto(jp.getDoubleValue());
        case VALUE_STRING:
          return new StringAttributeValueDto(jp.getText());
        case VALUE_TRUE:
          return new BooleanAttributeValueDto(jp.getBooleanValue());
        case VALUE_FALSE:
          return new BooleanAttributeValueDto(jp.getBooleanValue());
        case START_ARRAY: {
          ArrayAttributeValueDto arrayValue = null;
          ObjectMapper mapper = new ObjectMapper();
          ArrayNode arrayNode = mapper.readTree(jp);
          Iterator<JsonNode> it = arrayNode.elements();
          JsonNodeType arrayElemType = null;
          while (it.hasNext()) {
            JsonNode node = it.next();
            if (arrayValue == null) {
              // get the first element type
              arrayElemType = node.getNodeType();
              switch (arrayElemType) {
                case STRING:
                  arrayValue = new ArrayOfStringsAttributeValueDto();
                  break;
                case NUMBER:
                  arrayValue = new ArrayOfDoublesAttributeValueDto();
                  break;
                default:
                  throw new UnsupportedOperationException(
                      "Unsupported array attribute items type.");
              }
            } else if (arrayElemType != node.getNodeType()) {
              String errString = String.format(
                  "Array doesn't support different element types!Expected:{}, Found:{} ",
                  arrayElemType, node.getNodeType());
              LOGGER.warn(errString);
              throw new UnsupportedOperationException(errString);
            }
            switch (arrayElemType) {
              case STRING:
                ((ArrayOfStringsAttributeValueDto) arrayValue).add(node.asText());
                break;
              case NUMBER:
                ((ArrayOfDoublesAttributeValueDto) arrayValue).add(node.asDouble());
                break;
              default:
                break;
            }
          }
          return arrayValue;
        }

        default:
          break;
      }
    }

    throw new UnsupportedOperationException("Not supported attribute value yet.");
  }

}
