package com.softavail.commsrouter.api.dto.arg;

import com.softavail.commsrouter.api.dto.model.RouterObject;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;

/**
 * Created by @author mapuo on 18.09.17.
 */
public class UpdateTaskContext extends RouterObject {

  private AttributeGroupDto userContext;

  public UpdateTaskContext() {
  }

  public UpdateTaskContext(RouterObject rhs) {
    super(rhs);
  }

  public AttributeGroupDto getUserContext() {
    return userContext;
  }

  public void setUserContext(
      AttributeGroupDto userContext) {
    this.userContext = userContext;
  }

}
