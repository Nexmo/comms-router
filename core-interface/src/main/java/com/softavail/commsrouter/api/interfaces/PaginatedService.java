package com.softavail.commsrouter.api.interfaces;

import com.softavail.commsrouter.api.dto.misc.PaginatedList;
import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.exception.CommsRouterException;

/**
 * Created by @author mapuo on 10.10.17.
 */
public interface PaginatedService<DTOENTITYT> extends ApiObjectService<DTOENTITYT> {

  String TOTAL_COUNT_HEADER = "X-Total-Count";
  String PAGE_NUMBER_PARAM = "page_num";
  String ITEMS_PER_PAGE_PARAM = "per_page";

  PaginatedList<DTOENTITYT> list(PagingRequest request)
      throws CommsRouterException;

}
