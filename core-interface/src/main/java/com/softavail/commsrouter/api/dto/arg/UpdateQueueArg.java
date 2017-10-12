package com.softavail.commsrouter.api.dto.arg;

/**
 * Created by @author mapuo on 05.09.17.
 */
public class UpdateQueueArg {

  private String predicate;
  private String description;

  public String getPredicate() {
    return predicate;
  }

  public void setPredicate(String predicate) {
    this.predicate = predicate;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

}
