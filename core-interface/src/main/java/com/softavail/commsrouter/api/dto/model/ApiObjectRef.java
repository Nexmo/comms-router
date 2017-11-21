/* 
 * Copyright 2017 SoftAvail Inc.
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

package com.softavail.commsrouter.api.dto.model;

public class ApiObjectRef {

  private String ref;

  public ApiObjectRef() {}

  public ApiObjectRef(String id) {
    setRef(id);
  }

  public ApiObjectRef(ApiObjectRef rhs) {
    setRef(rhs.getRef());
  }

  public String getRef() {
    return ref;
  }

  public void setRef(String ref) {
    this.ref = ref;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ApiObjectId{");
    sb.append("id='").append(ref).append('\'');
    sb.append('}');
    return sb.toString();
  }

}
