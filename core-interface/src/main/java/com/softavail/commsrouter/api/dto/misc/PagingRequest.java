package com.softavail.commsrouter.api.dto.misc;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class PagingRequest {

  private final int page;
  private final int perPage;

  public PagingRequest(int page, int perPage) {
    this.page = page;
    this.perPage = perPage;
  }

  public int getPage() {
    return page;
  }

  public int getPerPage() {
    return perPage;
  }

}
