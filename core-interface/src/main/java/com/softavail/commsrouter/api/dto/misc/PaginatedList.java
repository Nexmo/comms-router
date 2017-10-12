package com.softavail.commsrouter.api.dto.misc;

import java.util.List;

/**
 * Created by @author mapuo on 05.09.17.
 */
public class PaginatedList<ENTITYT> {

  private List<ENTITYT> list;
  private int page;
  private int perPage;
  private long totalCount;

  public PaginatedList() {
  }

  public PaginatedList(PagingRequest request, List<ENTITYT> list, long totalCount) {
    this(list, request.getPage(), request.getPerPage(), totalCount);
  }

  public PaginatedList(List<ENTITYT> list, int page, int perPage, long totalCount) {
    this.list = list;
    this.page = page;
    this.perPage = perPage;
    this.totalCount = totalCount;
  }

  public List<ENTITYT> getList() {
    return list;
  }

  public void setList(List<ENTITYT> list) {
    this.list = list;
  }

  public int getPage() {
    return page;
  }

  public void setPage(int page) {
    this.page = page;
  }

  public int getPerPage() {
    return perPage;
  }

  public void setPerPage(int perPage) {
    this.perPage = perPage;
  }

  public long getTotalCount() {
    return totalCount;
  }

  public void setTotalCount(long totalCount) {
    this.totalCount = totalCount;
  }

}
