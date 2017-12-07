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

package com.softavail.commsrouter.api.dto.misc;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class PagingRequest {

  private final String routerRef;
  private final String token;
  private final int perPage;

  public PagingRequest() {
    this(null, 10); // TODO Setting?
  }

  public PagingRequest(String token, int perPage) {
    this(null, token, perPage);
  }

  public PagingRequest(String routerRef, String token, int perPage) {
    this.routerRef = routerRef;
    this.token = token;
    this.perPage = perPage;
  }

  public boolean isRouterRefAvailable() {
    return routerRef != null;
  }

  public String getRouterRef() {
    return routerRef;
  }

  public String getToken() {
    return token;
  }

  public int getPerPage() {
    return perPage;
  }

  @Override
  public String toString() {
    return new StringBuilder("PagingRequest{")
        .append("routerRef='").append(routerRef).append('\'')
        .append(", token='").append(token).append('\'')
        .append(", perPage=").append(perPage)
        .append('}').toString();
  }

}
