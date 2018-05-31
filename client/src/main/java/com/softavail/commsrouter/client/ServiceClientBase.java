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

package com.softavail.commsrouter.client;

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.interfaces.PaginatedService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.util.List;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04.09.17.
 */
public abstract class ServiceClientBase<T extends ApiObjectRef, R extends ApiObjectRef> {

  private static final Logger LOGGER = LogManager.getLogger(ServiceClientBase.class);

  private final Class<T> responseType;
  private final Class<R> createResponseType;

  @SuppressWarnings("unchecked")
  public ServiceClientBase() {
    Type tp = getClass().getGenericSuperclass();
    ParameterizedType pt = (ParameterizedType) tp;
    this.responseType = (Class<T>) (pt.getActualTypeArguments()[0]);
    this.createResponseType = (Class<R>) (pt.getActualTypeArguments()[1]);
  }

  // POST over container creates. Returns object
  protected R post(Object obj) {
    URI uri = getApiUrl().clone()
        .build();

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .post(Entity.entity(obj, MediaType.APPLICATION_JSON_TYPE), createResponseType);
  }

  // POST over container creates. Returns object
  protected R post(Object obj, String containerId) {
    URI uri = getApiUrl().clone()
        .build(containerId);

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .post(Entity.entity(obj, MediaType.APPLICATION_JSON_TYPE), createResponseType);
  }

  // POST over resource updates. Returns void
  protected void post(Object obj, ApiObjectRef ref) {
    URI uri = getApiUrl().clone()
        .path("{resourceRef}")
        .build(ref.getRef());

    getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .post(Entity.entity(obj, MediaType.APPLICATION_JSON_TYPE));
  }

  // POST over resource updates. Returns void
  protected void post(Object obj, RouterObjectRef ref) {
    URI uri = getApiUrl().clone()
        .path("{resourceRef}")
        .build(ref.getRouterRef(), ref.getRef());

    getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .post(Entity.entity(obj, MediaType.APPLICATION_JSON_TYPE));
  }

  protected R put(Object obj, String ref) {
    URI uri = getApiUrl().clone().build(ref);
    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .put(Entity.entity(obj, MediaType.APPLICATION_JSON_TYPE), createResponseType);
  }

  protected R put(Object obj, RouterObjectRef ref) {
    URI uri = getApiUrl().clone()
        .path("{resourceRef}")
        .build(ref.getRouterRef(), ref.getRef());

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .put(Entity.entity(obj, MediaType.APPLICATION_JSON_TYPE), createResponseType);
  }

  protected T getItem(ApiObjectRef ref) {
    URI uri = getApiUrl().clone().path(ref.getRef()).build();

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .get(responseType);
  }

  protected T getItem(RouterObjectRef ref) {
    URI uri = getApiUrl().clone()
        .path("{resourceId}").build(ref.getRouterRef(), ref.getRef());

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .get(responseType);
  }

  protected List<T> getList() {
    return getClient()
        .target(getApiUrl().clone())
        .request(MediaType.APPLICATION_JSON_TYPE)
        .get(new GenericType<List<T>>() {});
  }

  protected PaginatedList<T> getList(PagingRequest request) {
    UriBuilder uriBuilder = getApiUrl().clone()
        .queryParam(PaginatedService.TOKEN_PARAM, request.getToken())
        .queryParam(PaginatedService.ITEMS_PER_PAGE_PARAM, request.getPerPage())
        .queryParam(PaginatedService.SORT_PARAM, request.getSort())
        .queryParam(PaginatedService.QUERY_PARAM, request.getQuery());
    URI uri;
    if (request.isRouterRefAvailable()) {
      uri = uriBuilder.build(request.getRouterRef());
    } else {
      uri = uriBuilder.build();
    }

    Response response = getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .get();

    List<T> list = response.readEntity(new GenericType<List<T>>() {});
    String nextToken = response.getHeaderString(PaginatedService.NEXT_TOKEN_HEADER);

    return new PaginatedList<>(list, nextToken);
  }

  protected void deleteRequest(ApiObjectRef ref) {
    URI uri = getApiUrl().clone().path(ref.getRef()).build();

    getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .delete();
  }

  protected void deleteRequest(RouterObjectRef ref) {
    URI uri = getApiUrl().clone()
        .path("{resourceRef}")
        .build(ref.getRouterRef(), ref.getRef());

    getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .delete();
  }

  abstract UriBuilder getApiUrl();

  abstract Client getClient();
}
