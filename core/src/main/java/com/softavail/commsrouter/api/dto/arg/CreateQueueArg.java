package com.softavail.commsrouter.api.dto.arg;

import com.softavail.commsrouter.api.dto.model.RouterObject;

/**
 * Created by @author mapuo on 05.09.17.
 */
public class CreateQueueArg extends RouterObject {

  private String description;
  private String predicate;

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getPredicate() {
    return predicate;
  }

  public void setPredicate(String predicate) {
    this.predicate = predicate;
  }

}
