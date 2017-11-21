/*
 * Copyright 2017 SoftAvail Inc.
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

package com.softavail.commsrouter.domain;

import com.softavail.commsrouter.api.dto.model.ApiObjectRef;

import java.io.Serializable;
import java.util.Objects;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;
import javax.persistence.Version;

@MappedSuperclass
public class ApiObject implements Serializable {


  @Id
  private String ref;

  @Version
  private Integer version;

  public ApiObject() {}

  public ApiObject(ApiObject rhs) {
    this.ref = rhs.ref;
  }

  public ApiObject(String ref) {
    this.ref = ref;
  }

  public String getRef() {
    return ref;
  }

  public void setRef(String ref) {
    this.ref = ref;
  }

  public Integer getVersion() {
    return version;
  }

  public void setVersion(Integer version) {
    this.version = version;
  }

  public ApiObjectRef cloneApiObjectRef() {
    return new ApiObjectRef(ref);
  }

  @Override
  public boolean equals(Object object) {
    if (this == object) {
      return true;
    }
    if (object == null || getClass() != object.getClass()) {
      return false;
    }
    ApiObject apiObject = (ApiObject) object;
    return Objects.equals(getRef(), apiObject.getRef());
  }

  @Override
  public int hashCode() {
    return Objects.hash(getRef(), getVersion(), getClass());
  }

}
