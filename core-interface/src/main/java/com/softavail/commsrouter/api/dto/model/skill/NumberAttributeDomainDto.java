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

import java.util.List;
import java.util.Objects;

/**
 *
 * @author ikrustev
 */
public class NumberAttributeDomainDto extends AttributeDomainDto {

  private List<NumberInterval> intervals;

  public NumberAttributeDomainDto() {}

  public NumberAttributeDomainDto(List<NumberInterval> intervals) {
    this.intervals = intervals;
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof NumberAttributeDomainDto)) {
      return false;
    }
    NumberAttributeDomainDto rhs = (NumberAttributeDomainDto) obj;
    return Objects.equals(intervals, rhs.intervals);
  }

  @Override
  public int hashCode() {
    return Objects.hash(intervals);
  }

  @Override
  public AttributeType getType() {
    return AttributeType.number;
  }

  @Override
  public void accept(AttributeDomainDtoVisitor visitor) {
    visitor.handleNumberIntervals(intervals);
  }

  @Override
  public void validate() throws BadValueException {
    if (intervals == null) {
      return;
    }
    for (NumberInterval interval : intervals) {
      interval.validate();
    }
    int size = intervals.size();
    int count = 0;
    for (NumberInterval interval : intervals) {
      validateNoOverlap(interval, intervals.subList(++count, size));
    }
  }

  private static void validateNoOverlap(NumberInterval interval, List<NumberInterval> list)
          throws BadValueException {

    for (NumberInterval rhs : list) {
      if (interval.overlaps(rhs)) {
        throw new BadValueException("Intervals overlap: " + interval + " and " + rhs);
      }
    }
  }

  public List<NumberInterval> getIntervals() {
    return intervals;
  }

  public void setIntervals(List<NumberInterval> intervals) {
    this.intervals = intervals;
  }

}
