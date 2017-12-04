/*
 * Copyright 2017 SoftAvail, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.softavail.commsrouter.webservice.helpers;

import com.google.common.collect.Lists;

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.interfaces.RouterObjectService;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;
import javax.ws.rs.core.Link;
import javax.ws.rs.core.Link.Builder;
import javax.ws.rs.core.UriBuilder;

/**
 * Created by @author mapuo on 04/12/17.
 */
public class PaginationHelper {

  private final Supplier<UriBuilder> uriBuilderSupplier;
  private final Function<Builder, Link> linkCreator;

  public PaginationHelper(
      Supplier<UriBuilder> uriBuilderSupplier, Function<Link.Builder, Link> linkCreator) {

    this.uriBuilderSupplier = uriBuilderSupplier;
    this.linkCreator = linkCreator;
  }

  public Link[] getLinks(PaginatedList pagedList) {
    List<Link> result = Lists.newArrayList();

    int pageNum = pagedList.getPage();
    int perPage = pagedList.getPerPage();
    long totalCount = pagedList.getTotalCount();
    int maxPages = Math.toIntExact((totalCount + perPage - 1) / perPage);

    // Check first
    if (pageNum > 1) {
      result.add(createLink("first", 1, perPage));
    }

    // Check prev
    if (pageNum - 1 > 0) {
      result.add(createLink("prev", pageNum - 1, perPage));
    }

    // Check next
    if (pageNum + 1 <= maxPages) {
      result.add(createLink("next", pageNum + 1, perPage));
    }

    // Check last
    if (maxPages > 1 && pageNum < maxPages) {
      result.add(createLink("last", maxPages, perPage));
    }

    return result.toArray(new Link[result.size()]);
  }

  private Link createLink(String rel, int pageNum, int perPage) {
    UriBuilder uriBuilder = uriBuilderSupplier.get().clone()
        .queryParam(RouterObjectService.PAGE_NUMBER_PARAM, String.valueOf(pageNum));

    if (perPage != 10) {
      uriBuilder.queryParam(RouterObjectService.ITEMS_PER_PAGE_PARAM, String.valueOf(perPage));
    }

    return linkCreator.apply(Link.fromUriBuilder(uriBuilder).rel(rel));
  }

}
