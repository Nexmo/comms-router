/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model.attribute;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

import java.io.IOException;


/**
 * Custom serializer for AttributeValueDto.
 *
 * @author ergyunsyuleyman
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

    av.accept(new AttributeValueVisitor() {
      @Override
      public void handleBooleanValue(BooleanAttributeValueDto value) throws IOException {
        gen.writeBoolean(value.getValue());
      }

      @Override
      public void handleLongValue(LongAttributeValueDto value) throws IOException {
        gen.writeNumber(value.getValue());
      }

      @Override
      public void handleStringValue(StringAttributeValueDto value) throws IOException {
        gen.writeString(value.getValue());
      }

      @Override
      public void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value)
          throws IOException {
        gen.writeStartArray();
        for (Object object : value.getValue()) {
          gen.writeObject(object);
        }
        gen.writeEndArray();
      }

      @Override
      public void handleArrayOfLongsValue(ArrayOfLongsAttributeValueDto value) throws IOException {
        gen.writeStartArray();
        for (Object object : value.getValue()) {
          gen.writeObject(object);
        }
        gen.writeEndArray();
      }

      @Override
      public void handleArrayOfBooleansValue(ArrayOfBooleansAttributeValueDto value)
          throws IOException {
        gen.writeStartArray();
        for (Object object : value.getValue()) {
          gen.writeObject(object);
        }
        gen.writeEndArray();
      }

    });
  }

}
