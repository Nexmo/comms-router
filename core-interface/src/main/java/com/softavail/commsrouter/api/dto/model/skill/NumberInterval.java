/*
 * Copyright 2017 - 2018 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.softavail.commsrouter.api.dto.model.skill;

import com.softavail.commsrouter.api.exception.BadValueException;

import java.util.Objects;

/**
 *
 * @author ikrustev
 */
public class NumberInterval {

  private NumberIntervalBoundary low;
  private NumberIntervalBoundary high;

  public NumberInterval() {}

  public NumberInterval(NumberIntervalBoundary low, NumberIntervalBoundary high) {
    this.low = low;
    this.high = high;
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof NumberInterval)) {
      return false;
    }
    NumberInterval rhs = (NumberInterval) obj;
    return Objects.equals(low, rhs.low) && Objects.equals(high, rhs.high);
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.low, this.high);
  }

  @Override
  public String toString() {
    return lowToString() + ", " + highToString();
  }

  private String lowToString() {
    if (low == null) {
      return "null";
    }
    return (low.isInclusive() ? "[" : "(") + low.getBoundary().toString();
  }

  private String highToString() {
    if (high == null) {
      return "null";
    }
    return high.getBoundary().toString() + (high.isInclusive() ? "]" : ")");
  }

  public void validate() throws BadValueException {
    if (low == null) {
      throw new BadValueException("NumberInterval.low is required");
    }
    if (high == null) {
      throw new BadValueException("NumberInterval.high is required");
    }
    low.validate();
    high.validate();

    int compareLowHigh = low.compareBoundaryTo(high);

    if (compareLowHigh > 0) {
      throw new BadValueException("NumberInterval.low > NumberInterval.high");
    }

    if (compareLowHigh == 0 && (!low.isInclusive() || !high.isInclusive())) {
      throw new BadValueException("NumberInterval: when low = high both ends must be inclusive");
    }
  }

  public boolean overlaps(NumberInterval rhs) {
    return !doesNotOverlap(rhs);
  }

  public boolean doesNotOverlap(NumberInterval rhs) {
    int compareLowRhsToHigh = low.compareBoundaryTo(rhs.getHigh());
    if (compareLowRhsToHigh > 0
        || compareLowRhsToHigh == 0 && (!low.isInclusive() || !rhs.getHigh().isInclusive())) {
      return true;
    }
    int compareRhsLowToHigh = rhs.getLow().compareBoundaryTo(high);
    return compareRhsLowToHigh > 0
        || compareRhsLowToHigh == 0 && (!rhs.getLow().isInclusive() || !high.isInclusive());
  }

  public boolean contains(Double value) {
    if (low.isInclusive() && high.isInclusive()) {
      if (low.getBoundary() <= value && value <= high.getBoundary()) {
        return true;
      }
    }
    if (low.isInclusive() && !high.isInclusive()) {
      if (low.getBoundary() <= value && value < high.getBoundary()) {
        return true;
      }
    }
    if (!low.isInclusive() && high.isInclusive()) {
      if (low.getBoundary() < value && value <= high.getBoundary()) {
        return true;
      }
    }
    if (!low.isInclusive() && !high.isInclusive()) {
      if (low.getBoundary() < value && value < high.getBoundary()) {
        return true;
      }
    }
    return false;
  }

  public NumberIntervalBoundary getLow() {
    return low;
  }

  public void setLow(NumberIntervalBoundary low) {
    this.low = low;
  }

  public NumberIntervalBoundary getHigh() {
    return high;
  }

  public void setHigh(NumberIntervalBoundary high) {
    this.high = high;
  }

}
