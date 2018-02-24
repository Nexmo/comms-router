/*
 * Copyright 2018 ikrustev.
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

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.hamcrest.collection.IsIterableContainingInOrder;
import static org.junit.Assert.assertThat;
import org.junit.Test;

/**
 *
 * @author ikrustev
 */
public class SkillValueDomainDeserializerTest {

  private ObjectMapper objectMapper = new ObjectMapper();

  private static class TestBean {

    public AttributeValueDomainDto domain;

    public TestBean(AttributeValueDomainDto domain) {
      this.domain = domain;
    }

    public TestBean() {
    }

  }

  @Test(expected = JsonParseException.class)
  public void testInvalidJson() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{:"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      throw ex.getCause();
    }
  }

  @Test
  public void testInvalidDomainValue() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":false"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Value must be JSON object"));
    }
  }

  @Test
  public void testUnknownDomainField() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"some-unknown-field\":\"a value\""
              + "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Unknown field:"));
    }
  }

  @Test
  public void testMissingType() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Field 'type' is required"));
    }
  }

  @Test
  public void testBadType() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"bad-type\""
              + "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Invalid domain type"));
    }
  }

  @Test
  public void testDuplicateType() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"enumeration\","
                + "\"type\":\"enumeration\""
              + "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Duplicate field: type"));
    }
  }

  @Test
  public void testDuplicateValues() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"values\":[\"en\",\"es\"],"
                + "\"values\":[\"en\",\"es\"]"
              + "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Duplicate field: values"));
    }
  }

  @Test
  public void testDuplicateIntervals() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"intervals\":[{\"low\":{\"boundary\":13.4},\"high\":{\"boundary\":\"Infinity\"}}],"
                + "\"intervals\":[{\"low\":{\"boundary\":13.4},\"high\":{\"boundary\":\"Infinity\"}}]"
              + "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Duplicate field: intervals"));
    }
  }

  @Test
  public void testDuplicateRegex() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"regex\":\"\","
                + "\"regex\":\"\""
              + "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Duplicate field: regex"));
    }
  }

  @Test
  public void testEnumerationMissingValues() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"enumeration\""
              + "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Field 'values' is required for domain 'enumeration'"));
    }
  }

  @Test
  public void testEnumerationExtraIntervals() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"enumeration\","
                + "\"values\":[\"en\",\"es\"],"
                + "\"intervals\":[]"
+ "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Unknown field for domain 'enumeration': intervals"));
    }
  }

  @Test
  public void testEnumerationExtraRegex() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"enumeration\","
                + "\"values\":[\"en\",\"es\"],"
                + "\"regex\":\"\""
+ "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Unknown field for domain 'enumeration': regex"));
    }
  }

  @Test
  public void testEnumerationEmptyValues() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"enumeration\","
                + "\"values\":[]"
              + "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Field 'values' for domain 'enumeration' requires at least one element"));
    }
  }

  @Test
  public void testEnumeration() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"enumeration\","
                + "\"values\":[\"en\",\"es\"]"
              + "}"
            + "}";
    TestBean testBean = objectMapper.readValue(content, TestBean.class);
    assertEquals(AttributeValueType.enumeration, testBean.domain.getType());
    assertEquals(
            new HashSet<>(Arrays.asList("en", "es")),
            ((EnumerationAttributeValueDomainDto)testBean.domain).getValues()
    );
  }

  @Test
  public void testEnumerationSerializeDeserialize() throws IOException, Throwable {
    EnumerationAttributeValueDomainDto expected = new EnumerationAttributeValueDomainDto();
    expected.setValues(new HashSet<>(Arrays.asList("some", "enum", "value")));

    String asString = objectMapper.writeValueAsString(new TestBean(expected));

    TestBean actualTestBean = objectMapper.readValue(asString, TestBean.class);
    assertThat(actualTestBean.domain, instanceOf(EnumerationAttributeValueDomainDto.class));

    EnumerationAttributeValueDomainDto actual = (EnumerationAttributeValueDomainDto)actualTestBean.domain;

    assertEquals(expected.getType(), actual.getType());
    assertEquals(expected.getValues(), actual.getValues());
  }

  @Test
  public void testNumberExtraValues() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"number\","
                + "\"values\":[\"en\",\"es\"]"
+ "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Unknown field for domain 'number': values"));
    }
  }

  @Test
  public void testNumberExtraRegex() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"number\","
                + "\"regex\":\"\""
+ "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Unknown field for domain 'number': regex"));
    }
  }

  @Test
  public void testNumberNoIntervals() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"number\""
              + "}"
            + "}";
    TestBean testBean = objectMapper.readValue(content, TestBean.class);
    assertEquals(AttributeValueType.number, testBean.domain.getType());
    assertNull(((NumberAttributeValueDomainDto)testBean.domain).getIntervals());
  }

  @Test
  public void testNumber() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"number\","
                + "\"intervals\":[{\"low\":{\"boundary\":13.4},\"high\":{\"boundary\":\"Infinity\"}}]"
              + "}"
            + "}";
    TestBean testBean = objectMapper.readValue(content, TestBean.class);
    
    assertEquals(AttributeValueType.number, testBean.domain.getType());

    List<NumberInterval> actual = ((NumberAttributeValueDomainDto)testBean.domain).getIntervals();

    NumberInterval interval = new NumberInterval();
    interval.setLow(new NumberIntervalBoundary(13.4));
    interval.setHigh(NumberIntervalBoundary.POSITIVE_INFINITY);
    List<NumberInterval> expected = Arrays.asList(interval);
    assertThat(actual, IsIterableContainingInOrder.contains(expected.toArray()));
  }

  @Test
  public void testNumberSerializeDeserialize() throws IOException, Throwable {
    NumberInterval interval1 = new NumberInterval();
    interval1.setLow(NumberIntervalBoundary.NEGATIVE_INFINITY);
    interval1.setHigh(new NumberIntervalBoundary(10.0, true));

    NumberInterval interval2 = new NumberInterval();
    interval2.setLow(new NumberIntervalBoundary(13.4, false));
    interval2.setHigh(NumberIntervalBoundary.POSITIVE_INFINITY);

    NumberAttributeValueDomainDto expected = new NumberAttributeValueDomainDto();
    expected.setIntervals(Arrays.asList(interval1, interval2));

    String asString = objectMapper.writeValueAsString(new TestBean(expected));

    TestBean actualTestBean = objectMapper.readValue(asString, TestBean.class);
    assertThat(actualTestBean.domain, instanceOf(NumberAttributeValueDomainDto.class));

    NumberAttributeValueDomainDto actual = (NumberAttributeValueDomainDto)actualTestBean.domain;

    assertEquals(expected.getType(), actual.getType());
    assertThat(actual.getIntervals(),
            IsIterableContainingInOrder.contains(expected.getIntervals().toArray()));
  }

  @Test
  public void testStringExtraIntervals() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"string\","
                + "\"values\":[\"en\",\"es\"],"
                + "\"intervals\":[]"
+ "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Unknown field for domain 'string': intervals"));
    }
  }

  @Test
  public void testStringExtraValues() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"string\","
                + "\"values\":[\"en\",\"es\"]"
+ "}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      Throwable cause = ex.getCause();
      assertThat(cause, instanceOf(IllegalArgumentException.class));
      assertThat(cause.getMessage(), containsString("Unknown field for domain 'string': values"));
    }
  }

  @Test
  public void testStringNoRegex() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"string\""
              + "}"
            + "}";
    TestBean testBean = objectMapper.readValue(content, TestBean.class);

    assertEquals(AttributeValueType.string, testBean.domain.getType());
    assertNull(((StringAttributeValueDomainDto)testBean.domain).getRegex());
  }

  @Test
  public void testString() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"string\","
                + "\"regex\":\"prefix-.*\""
              + "}"
            + "}";
    TestBean testBean = objectMapper.readValue(content, TestBean.class);

    assertEquals(AttributeValueType.string, testBean.domain.getType());
    assertEquals("prefix-.*", ((StringAttributeValueDomainDto)testBean.domain).getRegex());
  }

  @Test
  public void testStringSerializeDeserialize() throws IOException, Throwable {
    StringAttributeValueDomainDto expected = new StringAttributeValueDomainDto();
    expected.setRegex("prefix-.*");

    String asString = objectMapper.writeValueAsString(new TestBean(expected));

    TestBean actualTestBean = objectMapper.readValue(asString, TestBean.class);
    assertThat(actualTestBean.domain, instanceOf(StringAttributeValueDomainDto.class));

    StringAttributeValueDomainDto actual = (StringAttributeValueDomainDto)actualTestBean.domain;

    assertEquals(expected.getType(), actual.getType());
    assertEquals(expected.getRegex(), actual.getRegex());
  }

}
