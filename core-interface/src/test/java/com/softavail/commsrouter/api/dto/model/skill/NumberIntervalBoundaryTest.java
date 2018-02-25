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

import com.softavail.commsrouter.api.exception.BadValueException;
import static org.hamcrest.Matchers.comparesEqualTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author ikrustev
 */
public class NumberIntervalBoundaryTest {

  public NumberIntervalBoundaryTest() {
  }

  @Test
  public void testNotEqualToNull() {
    Object rhs = null;
    NumberIntervalBoundary instance = new NumberIntervalBoundary();
    assertFalse(instance.equals(rhs));
  }

  @Test
  public void testNotEqualToOtherClasses() {
    Object rhs = new Object();
    NumberIntervalBoundary instance = new NumberIntervalBoundary();
    assertFalse(instance.equals(rhs));
  }

  @Test
  public void testNotEqualToDifferentBoundarySameInclusion() {
    NumberIntervalBoundary rhs = new NumberIntervalBoundary(10.0);
    NumberIntervalBoundary instance = new NumberIntervalBoundary(11.0);
    assertFalse(instance.equals(rhs));
  }

  @Test
  public void testNotEqualToSameBoundaryDifferentInclusion() {
    NumberIntervalBoundary rhs = new NumberIntervalBoundary(10.0, true);
    NumberIntervalBoundary instance = new NumberIntervalBoundary(10.0, false);
    assertFalse(instance.equals(rhs));
  }

  @Test
  public void testNotEqualToDifferentBoundaryDifferentInclusion() {
    NumberIntervalBoundary rhs = new NumberIntervalBoundary(10.0, true);
    NumberIntervalBoundary instance = new NumberIntervalBoundary(12.0, null);
    assertFalse(instance.equals(rhs));
  }

  @Test
  public void testEquals() {
    NumberIntervalBoundary rhs = new NumberIntervalBoundary(10.0, true);
    NumberIntervalBoundary instance = new NumberIntervalBoundary(10.0, true);
    assertTrue(instance.equals(rhs));
  }

  @Test
  public void testHashCodeForEqualInstances() {
    int lhs = new NumberIntervalBoundary(1.1, Boolean.FALSE).hashCode();
    int rhs = new NumberIntervalBoundary(1.1, Boolean.FALSE).hashCode();
    assertEquals(lhs, rhs);
  }

  @Test
  public void testHashCodeDiffersForDifferentBoundary() {
    NumberIntervalBoundary lhs = new NumberIntervalBoundary(1.13, Boolean.FALSE);
    NumberIntervalBoundary rhs = new NumberIntervalBoundary(2.13, lhs.getInclusive());
    assertNotEquals(lhs.hashCode(), rhs.hashCode());
  }

  @Test
  public void testHashCodeDiffersForDifferentInclusion() {
    NumberIntervalBoundary lhs = new NumberIntervalBoundary(8.13, Boolean.FALSE);
    NumberIntervalBoundary rhs = new NumberIntervalBoundary(lhs.getBoundary(), Boolean.TRUE);
    assertNotEquals(lhs.hashCode(), rhs.hashCode());
  }

  @Test
  public void testNotIsInclusiveByDefault() {
    NumberIntervalBoundary instance = new NumberIntervalBoundary();
    assertFalse(instance.isInclusive());
  }

  @Test
  public void testNotIsInclusiveWhenConstructedWithFalse() {
    NumberIntervalBoundary instance = new NumberIntervalBoundary(0.0, false);
    assertFalse(instance.isInclusive());
  }

  @Test
  public void testNotIsInclusiveWhenConstructedWithNull() {
    NumberIntervalBoundary instance = new NumberIntervalBoundary(0.0, null);
    assertFalse(instance.isInclusive());
  }

  @Test
  public void testIsInclusiveWhenConstructedWithTrue() {
    NumberIntervalBoundary instance = new NumberIntervalBoundary(0.0, true);
    assertTrue(instance.isInclusive());
  }

  @Test
  public void testNotIsInclusiveWhenSetToFalse() {
    NumberIntervalBoundary instance = new NumberIntervalBoundary();
    instance.setInclusive(Boolean.FALSE);
    assertFalse(instance.isInclusive());
  }

  @Test
  public void testNotIsInclusiveWhenSetToNull() {
    NumberIntervalBoundary instance = new NumberIntervalBoundary(0.0, null);
    instance.setInclusive(null);
    assertFalse(instance.isInclusive());
  }

  @Test
  public void testIsInclusiveWhenSetToTrue() {
    NumberIntervalBoundary instance = new NumberIntervalBoundary(0.0, true);
    instance.setInclusive(Boolean.TRUE);
    assertTrue(instance.isInclusive());
  }

  @Test
  public void testCompareBoundaryTo() {
    NumberIntervalBoundary instance = new NumberIntervalBoundary(0.0);
    assertThat(instance.compareBoundaryTo(new NumberIntervalBoundary(0.0)), comparesEqualTo(0));
    assertThat(instance.compareBoundaryTo(new NumberIntervalBoundary(-5.0)), greaterThan(0));
    assertThat(instance.compareBoundaryTo(NumberIntervalBoundary.NEGATIVE_INFINITY), greaterThan(0));
    assertThat(instance.compareBoundaryTo(new NumberIntervalBoundary(5.0)), lessThan(0));
    assertThat(instance.compareBoundaryTo(NumberIntervalBoundary.POSITIVE_INFINITY), lessThan(0));
  }

  @Test
  public void testValidateValid() throws Exception {
    NumberIntervalBoundary instance = new NumberIntervalBoundary(0.0);
    instance.validate();
  }

  @Test(expected = BadValueException.class)
  public void testValidateInvalid() throws Exception {
    NumberIntervalBoundary instance = new NumberIntervalBoundary();
    instance.validate();
  }

}
