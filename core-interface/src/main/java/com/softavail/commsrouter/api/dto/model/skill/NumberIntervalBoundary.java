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

/**
 *
 * @author ikrustev
 */
public class NumberIntervalBoundary {

  public static NumberIntervalBoundary POSITIVE_INFINITY
          = new NumberIntervalBoundary(Double.POSITIVE_INFINITY);

  public static NumberIntervalBoundary NEGATIVE_INFINITY
          = new NumberIntervalBoundary(Double.NEGATIVE_INFINITY);

  private Double boundary;
  private Boolean inclusive;

  public NumberIntervalBoundary() {}

  public NumberIntervalBoundary(Double boundary) {
    this.boundary = boundary;
  }

  public NumberIntervalBoundary(Double boundary, Boolean inclusive) {
    this.boundary = boundary;
    this.inclusive = inclusive;
  }

  public Double getBoundary() {
    return boundary;
  }

  public void setBoundary(Double boundary) {
    this.boundary = boundary;
  }

  public Boolean isInclusive() {
    return inclusive;
  }

  public void setInclusive(Boolean inclusive) {
    this.inclusive = inclusive;
  }

}
