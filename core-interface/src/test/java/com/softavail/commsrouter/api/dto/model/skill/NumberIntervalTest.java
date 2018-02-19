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

import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author ikrustev
 */
public class NumberIntervalTest {

  public NumberIntervalTest() {
  }

  @Test
  public void testEqualsNotEqualToNull() {
    Object rhs = null;
    NumberInterval instance = new NumberInterval();
    assertFalse(instance.equals(rhs));
  }

  @Test
  public void testEqualsNotEqualToOtherClasses() {
    Object rhs = new Object();
    NumberInterval instance = new NumberInterval();
    assertFalse(instance.equals(rhs));
  }

  @Test
  public void testNotEqualToSameLowDifferentHigh() {
    NumberInterval rhs = new NumberInterval();
    rhs.setLow(NumberIntervalBoundary.NEGATIVE_INFINITY);
    rhs.setHigh(NumberIntervalBoundary.POSITIVE_INFINITY);
    NumberInterval instance = new NumberInterval();
    instance.setLow(rhs.getLow());
    instance.setHigh(null);
    assertFalse(instance.equals(rhs));
  }

  @Test
  public void testNotEqualToDifferentLowSameHigh() {
    NumberInterval rhs = new NumberInterval();
    rhs.setLow(NumberIntervalBoundary.NEGATIVE_INFINITY);
    rhs.setHigh(NumberIntervalBoundary.POSITIVE_INFINITY);
    NumberInterval instance = new NumberInterval();
    instance.setLow(NumberIntervalBoundary.POSITIVE_INFINITY);
    instance.setHigh(rhs.getHigh());
    assertFalse(instance.equals(rhs));
  }

  @Test
  public void testNotEqualToDifferentLowDifferentHigh() {
    NumberInterval rhs = new NumberInterval();
    rhs.setLow(NumberIntervalBoundary.NEGATIVE_INFINITY);
    rhs.setHigh(NumberIntervalBoundary.POSITIVE_INFINITY);
    NumberInterval instance = new NumberInterval();
    instance.setLow(new NumberIntervalBoundary(0.0));
    instance.setHigh(new NumberIntervalBoundary(10.0));
    assertFalse(instance.equals(rhs));
  }

  @Test
  public void testEquals() {
    NumberInterval rhs = new NumberInterval();
    rhs.setLow(new NumberIntervalBoundary(0.0));
    rhs.setHigh(new NumberIntervalBoundary(10.0));
    NumberInterval instance = new NumberInterval();
    instance.setLow(new NumberIntervalBoundary(0.0));
    instance.setHigh(new NumberIntervalBoundary(10.0));
    assertTrue(instance.equals(rhs));
  }

  @Test
  public void testHashCodeForEqualInstances() {
    NumberInterval lhs = new NumberInterval();
    lhs.setLow(NumberIntervalBoundary.NEGATIVE_INFINITY);
    lhs.setHigh(new NumberIntervalBoundary(0.0));
    NumberInterval rhs = new NumberInterval();
    rhs.setLow(NumberIntervalBoundary.NEGATIVE_INFINITY);
    rhs.setHigh(new NumberIntervalBoundary(0.0));
    assertEquals(lhs.hashCode(), rhs.hashCode());
  }

  @Test
  public void testHashCodeDifferesForDifferentLows() {
    NumberInterval lhs = new NumberInterval();
    lhs.setLow(new NumberIntervalBoundary(0.0));
    lhs.setHigh(NumberIntervalBoundary.POSITIVE_INFINITY);
    NumberInterval rhs = new NumberInterval();
    rhs.setLow(NumberIntervalBoundary.NEGATIVE_INFINITY);
    rhs.setHigh(NumberIntervalBoundary.POSITIVE_INFINITY);
    assertNotEquals(lhs.hashCode(), rhs.hashCode());
  }

  @Test
  public void testHashCodeDifferesForDifferentHighs() {
    NumberInterval lhs = new NumberInterval();
    lhs.setLow(NumberIntervalBoundary.NEGATIVE_INFINITY);
    lhs.setHigh(new NumberIntervalBoundary(0.0));
    NumberInterval rhs = new NumberInterval();
    rhs.setLow(NumberIntervalBoundary.NEGATIVE_INFINITY);
    rhs.setHigh(NumberIntervalBoundary.POSITIVE_INFINITY);
    assertNotEquals(lhs.hashCode(), rhs.hashCode());
  }

}
