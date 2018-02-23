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
  public boolean equals(Object o) {
    if (!(o instanceof NumberInterval)) {
      return false;
    }
    NumberInterval rhs = (NumberInterval)o;
    return Objects.equals(low, rhs.low) && Objects.equals(high, rhs.high);
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.low, this.high);
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
