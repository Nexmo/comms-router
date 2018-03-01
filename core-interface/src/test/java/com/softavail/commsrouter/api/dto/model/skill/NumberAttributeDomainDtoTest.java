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
import java.util.Arrays;
import org.junit.Test;

/**
 *
 * @author ikrustev
 */
public class NumberAttributeDomainDtoTest {

  public NumberAttributeDomainDtoTest() {
  }

  @Test
  public void testValidateNoIntervals() throws Exception {
    NumberAttributeDomainDto instance = new NumberAttributeDomainDto();
    instance.validate();
  }

  @Test
  public void testValidateEmptyIntervals() throws Exception {
    NumberAttributeDomainDto instance = new NumberAttributeDomainDto(Arrays.asList());
    instance.validate();
  }

  @Test(expected = BadValueException.class)
  public void testValidateBadInterval() throws Exception {
    NumberAttributeDomainDto instance = new NumberAttributeDomainDto(Arrays.asList(
            new NumberInterval()
    ));
    instance.validate();
  }

  @Test
  public void testValidateSingle() throws Exception {
    NumberAttributeDomainDto instance = new NumberAttributeDomainDto(Arrays.asList(
            new NumberInterval(
                    NumberIntervalBoundary.NEGATIVE_INFINITY,
                    NumberIntervalBoundary.POSITIVE_INFINITY
            )
    ));
    instance.validate();
  }

  @Test(expected = BadValueException.class)
  public void testValidateOverlap() throws Exception {
    NumberAttributeDomainDto instance = new NumberAttributeDomainDto(Arrays.asList(
            new NumberInterval(
                    NumberIntervalBoundary.NEGATIVE_INFINITY,
                    NumberIntervalBoundary.POSITIVE_INFINITY
            ),
            new NumberInterval(
                    new NumberIntervalBoundary(10.0, true),
                    new NumberIntervalBoundary(10.0, true)
            )
    ));
    instance.validate();
  }

  @Test(expected = BadValueException.class)
  public void testValidateOverlapSwapped() throws Exception {
    NumberAttributeDomainDto instance = new NumberAttributeDomainDto(Arrays.asList(
            new NumberInterval(
                    new NumberIntervalBoundary(10.0, true),
                    new NumberIntervalBoundary(10.0, true)
            ),
            new NumberInterval(
                    NumberIntervalBoundary.NEGATIVE_INFINITY,
                    NumberIntervalBoundary.POSITIVE_INFINITY
            )
    ));
    instance.validate();
  }

  @Test
  public void testValidateTwo() throws Exception {
    NumberAttributeDomainDto instance = new NumberAttributeDomainDto(Arrays.asList(
            new NumberInterval(
                    new NumberIntervalBoundary(10.0, true),
                    new NumberIntervalBoundary(10.0, true)
            ),
            new NumberInterval(
                    new NumberIntervalBoundary(10.0, false),
                    new NumberIntervalBoundary(20.0, true)
            )
    ));
    instance.validate();
  }

  @Test
  public void testValidateTwoSwapped() throws Exception {
    NumberAttributeDomainDto instance = new NumberAttributeDomainDto(Arrays.asList(
            new NumberInterval(
                    new NumberIntervalBoundary(10.0, false),
                    new NumberIntervalBoundary(20.0, true)
            ),
            new NumberInterval(
                    new NumberIntervalBoundary(10.0, true),
                    new NumberIntervalBoundary(10.0, true)
            )
    ));
    instance.validate();
  }

}
