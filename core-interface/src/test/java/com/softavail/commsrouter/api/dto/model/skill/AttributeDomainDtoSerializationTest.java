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

import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.exc.InvalidTypeIdException;
import com.fasterxml.jackson.databind.exc.UnrecognizedPropertyException;
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
public class AttributeDomainDtoSerializationTest {

  private ObjectMapper objectMapper = new ObjectMapper();

  private static class TestBean {

    public AttributeDomainDto domain;

    public TestBean(AttributeDomainDto domain) {
      this.domain = domain;
    }

    public TestBean() {
    }

  }

  @Test(expected = JsonMappingException.class)
  public void testInvalidDomainValue() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":false"
            + "}";
    objectMapper.readValue(content, TestBean.class);
  }

  @Test
  public void testMissingType() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{}"
            + "}";
    try {
      objectMapper.readValue(content, TestBean.class);
    } catch (JsonMappingException ex) {
      assertThat(ex.getMessage(), containsString("missing property 'type'"));
    }
  }

  @Test(expected = UnrecognizedPropertyException.class)
  public void testUnknownProperty() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"enumeration\","
                + "\"some-unknown-field\":\"a value\""
              + "}"
            + "}";
    objectMapper.readValue(content, TestBean.class);
  }

  @Test(expected = InvalidTypeIdException.class)
  public void testBadType() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"bad-type\""
              + "}"
            + "}";
    objectMapper.readValue(content, TestBean.class);
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
    assertEquals(AttributeType.enumeration, testBean.domain.getType());
    assertEquals(
            new HashSet<>(Arrays.asList("en", "es")),
            ((EnumerationAttributeDomainDto)testBean.domain).getValues()
    );
  }

  @Test
  public void testEnumerationSerializeDeserialize() throws IOException, Throwable {
    EnumerationAttributeDomainDto expected = new EnumerationAttributeDomainDto();
    expected.setValues(new HashSet<>(Arrays.asList("some", "enum", "value")));

    String asString = objectMapper.writeValueAsString(new TestBean(expected));

    TestBean actualTestBean = objectMapper.readValue(asString, TestBean.class);
    assertThat(actualTestBean.domain, instanceOf(EnumerationAttributeDomainDto.class));

    EnumerationAttributeDomainDto actual = (EnumerationAttributeDomainDto)actualTestBean.domain;

    assertEquals(expected.getType(), actual.getType());
    assertEquals(expected.getValues(), actual.getValues());
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
    
    assertEquals(AttributeType.number, testBean.domain.getType());

    List<NumberInterval> actual = ((NumberAttributeDomainDto)testBean.domain).getIntervals();

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

    NumberAttributeDomainDto expected = new NumberAttributeDomainDto();
    expected.setIntervals(Arrays.asList(interval1, interval2));

    String asString = objectMapper.writeValueAsString(new TestBean(expected));

    TestBean actualTestBean = objectMapper.readValue(asString, TestBean.class);
    assertThat(actualTestBean.domain, instanceOf(NumberAttributeDomainDto.class));

    NumberAttributeDomainDto actual = (NumberAttributeDomainDto)actualTestBean.domain;

    assertEquals(expected.getType(), actual.getType());
    assertThat(actual.getIntervals(),
            IsIterableContainingInOrder.contains(expected.getIntervals().toArray()));
  }

  @Test
  public void testStringNoRegex() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"string\""
              + "}"
            + "}";
    TestBean testBean = objectMapper.readValue(content, TestBean.class);

    assertEquals(AttributeType.string, testBean.domain.getType());
    assertNull(((StringAttributeDomainDto)testBean.domain).getRegex());
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

    assertEquals(AttributeType.string, testBean.domain.getType());
    assertEquals("prefix-.*", ((StringAttributeDomainDto)testBean.domain).getRegex());
  }

  @Test
  public void testStringSerializeDeserialize() throws IOException, Throwable {
    StringAttributeDomainDto expected = new StringAttributeDomainDto();
    expected.setRegex("prefix-.*");

    String asString = objectMapper.writeValueAsString(new TestBean(expected));

    TestBean actualTestBean = objectMapper.readValue(asString, TestBean.class);
    assertThat(actualTestBean.domain, instanceOf(StringAttributeDomainDto.class));

    StringAttributeDomainDto actual = (StringAttributeDomainDto)actualTestBean.domain;

    assertEquals(expected.getType(), actual.getType());
    assertEquals(expected.getRegex(), actual.getRegex());
  }

  @Test
  public void testBool() throws IOException, Throwable {
    String content = "{"
              + "\"domain\":{"
                + "\"type\":\"bool\""
              + "}"
            + "}";
    TestBean testBean = objectMapper.readValue(content, TestBean.class);

    assertEquals(AttributeType.bool, testBean.domain.getType());
  }

  @Test
  public void testBoolSerializeDeserialize() throws IOException, Throwable {
    BoolAttributeDomainDto expected = new BoolAttributeDomainDto();

    String asString = objectMapper.writeValueAsString(new TestBean(expected));

    TestBean actualTestBean = objectMapper.readValue(asString, TestBean.class);
    assertThat(actualTestBean.domain, instanceOf(BoolAttributeDomainDto.class));

    BoolAttributeDomainDto actual = (BoolAttributeDomainDto)actualTestBean.domain;

    assertEquals(expected.getType(), actual.getType());
  }

}
