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

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.softavail.commsrouter.api.exception.CommsRouterException;

import java.io.IOException;


/**
 *
 * @author Ergyun Syuleyman
 */
public class AttributeValueSerializer extends StdSerializer<AttributeValueDto> {

  /**
   * Generated serial version value.
   */
  private static final long serialVersionUID = -5267266872474097413L;

  public AttributeValueSerializer() {
    this(null);
  }


  public AttributeValueSerializer(Class<AttributeValueDto> avt) {
    super(avt);
  }

  @Override
  public void serialize(AttributeValueDto av, JsonGenerator gen, SerializerProvider sp)
      throws IOException {

    AttributeValueSerializerVisitor visitor = new AttributeValueSerializerVisitor(gen);
    try {
      av.accept(visitor);
    } catch (CommsRouterException ex) {
      throw (IOException) ex.getCause();
    }
    IOException ex = visitor.getException();
    if (ex != null) {
      throw new IOException("Attibute value serialization failure: " + ex.toString(), ex);
    }
  }

  private static class AttributeValueSerializerVisitor implements AttributeValueVisitor {

    private final JsonGenerator gen;
    private IOException exception;

    public AttributeValueSerializerVisitor(JsonGenerator gen) {
      this.gen = gen;
      this.exception = null;
    }

    public IOException getException() {
      return exception;
    }

    @Override
    public void handleBooleanValue(BooleanAttributeValueDto value) {
      try {
        gen.writeBoolean(value.getValue());
      } catch (IOException ex) {
        exception = ex;
      }
    }

    @Override
    public void handleDoubleValue(DoubleAttributeValueDto value) {
      try {
        gen.writeNumber(value.getValue());
      } catch (IOException ex) {
        exception = ex;
      }
    }

    @Override
    public void handleStringValue(StringAttributeValueDto value) {
      try {
        gen.writeString(value.getValue());
      } catch (IOException ex) {
        exception = ex;
      }
    }

    @Override
    public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value) {
      try {
        gen.writeStartArray();
        for (Object object : value.getValue()) {
          gen.writeObject(object);
        }
        gen.writeEndArray();
      } catch (IOException ex) {
        exception = ex;
      }
    }

    @Override
    public void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value) {
      try {
        gen.writeStartArray();
        for (Object object : value.getValue()) {
          gen.writeObject(object);
        }
        gen.writeEndArray();
      } catch (IOException ex) {
        exception = ex;
      }
    }
  }

}
