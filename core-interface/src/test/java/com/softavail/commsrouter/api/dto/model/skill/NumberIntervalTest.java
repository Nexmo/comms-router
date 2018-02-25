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

  @Test
  public void testToString() {
    NumberInterval instance;
    instance = new NumberInterval();
    assertEquals("null, null", instance.toString());
    instance = new NumberInterval(
            NumberIntervalBoundary.NEGATIVE_INFINITY,
            NumberIntervalBoundary.POSITIVE_INFINITY);
    assertEquals("(-Infinity, Infinity)", instance.toString());
    instance = new NumberInterval(
            new NumberIntervalBoundary(0.0, true),
            new NumberIntervalBoundary(10.3, false));
    assertEquals("[0.0, 10.3)", instance.toString());
    instance = new NumberInterval(
            new NumberIntervalBoundary(0.0, null),
            new NumberIntervalBoundary(10.3, true));
    assertEquals("(0.0, 10.3]", instance.toString());
  }

  @Test(expected = BadValueException.class)
  public void testValidateNoBoundaries() throws Exception {
    NumberInterval instance = new NumberInterval();
    instance.validate();
  }

  @Test(expected = BadValueException.class)
  public void testValidateNoLow() throws Exception {
    NumberInterval instance = new NumberInterval(null, NumberIntervalBoundary.POSITIVE_INFINITY);
    instance.validate();
  }

  @Test(expected = BadValueException.class)
  public void testValidateNoHigh() throws Exception {
    NumberInterval instance = new NumberInterval(NumberIntervalBoundary.NEGATIVE_INFINITY, null);
    instance.validate();
  }

  @Test(expected = BadValueException.class)
  public void testValidateHighLessThanLow() throws Exception {
    NumberInterval instance = new NumberInterval(
            new NumberIntervalBoundary(10.0),
            new NumberIntervalBoundary(0.0)
    );
    instance.validate();
  }

  @Test(expected = BadValueException.class)
  public void testValidateLowAndHighEqualLowNotInclusive() throws Exception {
    NumberInterval instance = new NumberInterval(
            new NumberIntervalBoundary(10.0),
            new NumberIntervalBoundary(10.0, true)
    );
    instance.validate();
  }

  @Test(expected = BadValueException.class)
  public void testValidateLowAndHighEqualHighNotInclusive() throws Exception {
    NumberInterval instance = new NumberInterval(
            new NumberIntervalBoundary(10.0, true),
            new NumberIntervalBoundary(10.0, false)
    );
    instance.validate();
  }

  @Test
  public void testValidateLowAndHighEqual() throws Exception {
    NumberInterval instance = new NumberInterval(
            new NumberIntervalBoundary(10.0, true),
            new NumberIntervalBoundary(10.0, true)
    );
    instance.validate();
  }

  @Test
  public void testValidateInfinity() throws Exception {
    NumberInterval instance = new NumberInterval(
            NumberIntervalBoundary.NEGATIVE_INFINITY,
            NumberIntervalBoundary.POSITIVE_INFINITY
    );
    instance.validate();
  }

  @Test
  public void testValidateLowInfinity() throws Exception {
    NumberInterval instance = new NumberInterval(
            NumberIntervalBoundary.NEGATIVE_INFINITY,
            new NumberIntervalBoundary(10.0, true)
    );
    instance.validate();
  }

  @Test
  public void testValidateHighInfinity() throws Exception {
    NumberInterval instance = new NumberInterval(
            new NumberIntervalBoundary(10.0, true),
            NumberIntervalBoundary.POSITIVE_INFINITY
    );
    instance.validate();
  }

  @Test
  public void testValidate() throws Exception {
    NumberInterval instance = new NumberInterval(
            new NumberIntervalBoundary(-10.0),
            new NumberIntervalBoundary(10.0, false)
    );
    instance.validate();
  }

  static private String msg(String message, NumberInterval lhs, NumberInterval rhs) {
    return message + lhs.toString() + " and " + rhs.toString();
  }

  static private void assertOverlapOneWayTest(NumberInterval lhs, NumberInterval rhs) {
    assertTrue(msg("Had to overlap: ", lhs, rhs), lhs.overlaps(rhs));
  }

  static private void assertOverlap(NumberInterval lhs, NumberInterval rhs) {
    assertOverlapOneWayTest(lhs, rhs);
    assertOverlapOneWayTest(rhs, lhs);
  }

  static private void assertDontOverlapOneWayTest(NumberInterval lhs, NumberInterval rhs) {
    assertFalse(msg("Must not overlap: ", lhs, rhs), lhs.overlaps(rhs));
  }

  static private void assertDontOverlap(NumberInterval lhs, NumberInterval rhs) {
    assertDontOverlapOneWayTest(lhs, rhs);
    assertDontOverlapOneWayTest(rhs, lhs);
  }

  private static void testOverlapCommon(NumberInterval lhs) {
    NumberInterval rhs;

    rhs = new NumberInterval(
            NumberIntervalBoundary.NEGATIVE_INFINITY,
            new NumberIntervalBoundary(-20.0)
    );
    assertDontOverlap(lhs, rhs);

    rhs = new NumberInterval(
            NumberIntervalBoundary.NEGATIVE_INFINITY,
            new NumberIntervalBoundary(-10.0)
    );
    assertDontOverlap(lhs, rhs);

    rhs = new NumberInterval(
            NumberIntervalBoundary.NEGATIVE_INFINITY,
            new NumberIntervalBoundary(0.0, true)
    );
    assertOverlap(lhs, rhs);

    rhs = new NumberInterval(
            new NumberIntervalBoundary(-5.0, true),
            new NumberIntervalBoundary(0.0, true)
    );
    assertOverlap(lhs, rhs);

    rhs = new NumberInterval(
            new NumberIntervalBoundary(-5.0, true),
            new NumberIntervalBoundary(-5.0, true)
    );
    assertOverlap(lhs, rhs);

    rhs = new NumberInterval(
            new NumberIntervalBoundary(-5.0, true),
            NumberIntervalBoundary.POSITIVE_INFINITY
    );
    assertOverlap(lhs, rhs);

    rhs = new NumberInterval(
            new NumberIntervalBoundary(10.0),
            NumberIntervalBoundary.POSITIVE_INFINITY
    );
    assertDontOverlap(lhs, rhs);

    rhs = new NumberInterval(
            new NumberIntervalBoundary(20.0, true),
            NumberIntervalBoundary.POSITIVE_INFINITY
    );
    assertDontOverlap(lhs, rhs);
  }

  @Test
  public void testOverlaps() {
    NumberInterval lhs;
    NumberInterval rhs;

    lhs = new NumberInterval(
            new NumberIntervalBoundary(-10.0),
            new NumberIntervalBoundary(10.0)
    );
    testOverlapCommon(lhs);
    rhs = new NumberInterval(
            NumberIntervalBoundary.NEGATIVE_INFINITY,
            new NumberIntervalBoundary(-10.0, true)
    );
    assertDontOverlap(lhs, rhs);
    rhs = new NumberInterval(
            new NumberIntervalBoundary(10.0, true),
            NumberIntervalBoundary.POSITIVE_INFINITY
    );
    assertDontOverlap(lhs, rhs);

    lhs = new NumberInterval(
            new NumberIntervalBoundary(-10.0, true),
            new NumberIntervalBoundary(10.0)
    );
    testOverlapCommon(lhs);
    rhs = new NumberInterval(
            NumberIntervalBoundary.NEGATIVE_INFINITY,
            new NumberIntervalBoundary(-10.0, true)
    );
    assertOverlap(lhs, rhs);
    rhs = new NumberInterval(
            new NumberIntervalBoundary(10.0, true),
            NumberIntervalBoundary.POSITIVE_INFINITY
    );
    assertDontOverlap(lhs, rhs);

    lhs = new NumberInterval(
            new NumberIntervalBoundary(-10.0),
            new NumberIntervalBoundary(10.0, true)
    );
    testOverlapCommon(lhs);
    rhs = new NumberInterval(
            NumberIntervalBoundary.NEGATIVE_INFINITY,
            new NumberIntervalBoundary(-10.0, true)
    );
    assertDontOverlap(lhs, rhs);
    rhs = new NumberInterval(
            new NumberIntervalBoundary(10.0, true),
            NumberIntervalBoundary.POSITIVE_INFINITY
    );
    assertOverlap(lhs, rhs);

    lhs = new NumberInterval(
            new NumberIntervalBoundary(-10.0, true),
            new NumberIntervalBoundary(10.0, true)
    );
    testOverlapCommon(lhs);
    rhs = new NumberInterval(
            NumberIntervalBoundary.NEGATIVE_INFINITY,
            new NumberIntervalBoundary(-10.0, true)
    );
    assertOverlap(lhs, rhs);
    rhs = new NumberInterval(
            new NumberIntervalBoundary(10.0, true),
            NumberIntervalBoundary.POSITIVE_INFINITY
    );
    assertOverlap(lhs, rhs);
  }

}
