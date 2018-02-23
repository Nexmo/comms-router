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

package com.softavail.commsrouter.api.dto.model.skill;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;

import java.io.IOException;
import java.util.List;
import java.util.Set;

/**
 *
 * @author ikrustev
 */
public class SkillValueDomainDeserializer extends StdDeserializer<AttributeValueDomainDto> {

  public SkillValueDomainDeserializer() {
    this(null);
  }

  public SkillValueDomainDeserializer(Class<?> vc) {
    super(vc);
  }

  @Override
  public AttributeValueDomainDto deserialize(JsonParser jp, DeserializationContext ctxt)
      throws IOException, JsonProcessingException {

    JsonToken currentToken = jp.getCurrentToken();
    if (currentToken != JsonToken.START_OBJECT) {
      throw new IllegalArgumentException("Value must be JSON object");
    }
    
    String type = null;
    Set<String> values = null;
    List<NumberInterval> intervals = null;
    String regex = null;

    for (;;) {
      String name = jp.nextFieldName();
      if (name == null) {
        break;
      }
      switch (name) {
        case "type":
          if (type != null) {
            throw new IllegalArgumentException("Duplicate field: type");
          }
          type = jp.nextTextValue();
          break;
        case "values":
          if (values != null) {
            throw new IllegalArgumentException("Duplicate field: values");
          }
          jp.nextValue();
          values = jp.readValueAs(new TypeReference<Set<String>>() {});
          break;
        case "intervals":
          if (intervals != null) {
            throw new IllegalArgumentException("Duplicate field: intervals");
          }
          jp.nextValue();
          intervals = jp.readValueAs(new TypeReference<List<NumberInterval>>(){});
          break;
        case "regex":
          if (regex != null) {
            throw new IllegalArgumentException("Duplicate field: regex");
          }
          regex = jp.nextTextValue();
          break;
        default:
          throw new IllegalArgumentException("Unknown field: " + name);
      }
    }
    if (type == null) {
      throw new IllegalArgumentException("Field 'type' is required");
    }

    switch (type) {
      case "enumeration":
        if (values == null) {
          throw new IllegalArgumentException("Field 'values' is required for domain 'enumeration'");
        }
        if (values.size() < 1) {
          throw new IllegalArgumentException("Field 'values' for domain 'enumeration' requires at least one element");
        }
        if (intervals != null) {
          throw new IllegalArgumentException("Unknown field for domain 'enumeration': intervals");
        }
        if (regex != null) {
          throw new IllegalArgumentException("Unknown field for domain 'enumeration': regex");
        }
        return new EnumerationAttributeValueDomainDto(values);
      case "number":
        if (values != null) {
          throw new IllegalArgumentException("Unknown field for domain 'number': values");
        }
        if (regex != null) {
          throw new IllegalArgumentException("Unknown field for domain 'number': regex");
        }
        return new NumberAttributeValueDomainDto(intervals);
      case "string":
        if (intervals != null) {
          throw new IllegalArgumentException("Unknown field for domain 'string': intervals");
        }
        if (values != null) {
          throw new IllegalArgumentException("Unknown field for domain 'string': values");
        }
        return new StringAttributeValueDomainDto(regex);
      default:
        throw new IllegalArgumentException("Invalid domain type: " + type);
    }
  }

}
