package com.softavail.comms.demo.application.client;

import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.model.RouterObject;

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
public abstract class ServiceClientBase<T> {

  private final Class<T> responseType;

  public ServiceClientBase(Class<T> responseType) {
    this.responseType = responseType;
  }

  protected T post(ApiObject o) {
    URI uri = getApiUrl().clone().build();
    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .post(Entity.entity(o, MediaType.APPLICATION_JSON_TYPE), responseType);
  }

  protected T post(RouterObject o) {
    URI uri = getApiUrl().clone()
        .build(o.getRouterId());

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .post(Entity.entity(o, MediaType.APPLICATION_JSON_TYPE), responseType);
  }

  protected void put(ApiObject o) {
    URI uri = getApiUrl().clone().build();
    getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .put(Entity.entity(o, MediaType.APPLICATION_JSON_TYPE));
  }

  protected void put(RouterObject o) {
    URI uri = getApiUrl().clone()
        .path("{resourceId}")
        .build(o.getRouterId(), o.getId());
    getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .put(Entity.entity(o, MediaType.APPLICATION_JSON_TYPE));
  }

  protected T getItem(String id) {
    URI uri = getApiUrl().clone().path(id).build();

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .get(responseType);
  }

  protected T getItem(RouterObject routerObject) {
    URI uri = getApiUrl().clone()
        .path("{resourceId}")
        .build(routerObject.getRouterId(), routerObject.getId());

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

  protected List<T> getList(String routerId) {
    URI uri = getApiUrl().clone()
        .build(routerId);

    return getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .get(new GenericType<List<T>>() {});
  }

  protected PaginatedList<T> getList(String routerId, int page, int perPage) {
    URI uri = getApiUrl().clone()
        .queryParam("page_num", page)
        .queryParam("per_page", perPage)
        .build(routerId);

    Response response = getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .get();

    List<T> list = response.readEntity(new GenericType<List<T>>() {});
    Long totalCount = Long.valueOf(response.getHeaderString("X-Total-Count"));

    return new PaginatedList<>(list, page, perPage, totalCount);
  }

  protected void delete(String id) {
    URI uri = getApiUrl().clone().path(id).build();

    getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .delete();
  }

  protected void delete(RouterObject routerObject) {
    URI uri = getApiUrl().clone()
        .path("{resourceId}")
        .build(routerObject.getRouterId(), routerObject.getId());

    getClient()
        .target(uri)
        .request(MediaType.APPLICATION_JSON_TYPE)
        .delete();
  }

  abstract UriBuilder getApiUrl();

  abstract Client getClient();

}
