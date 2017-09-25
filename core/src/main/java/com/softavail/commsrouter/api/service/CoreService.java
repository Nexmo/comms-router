package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.util.Uuid;

/**
 * Created by @author mapuo on 20.09.17.
 */
public class CoreService {

  /**
   * Assign unique ID to apiObject if there is not one already
   *
   * @param apiObject the object that will be mutated if id is null
   * @param <T> object that extends ApiObject
   * @return the same object or mutated object if id is null
   */
  protected <T extends ApiObject> T ensureIdPresent(T apiObject) {
    if (apiObject.getId() == null) {
      apiObject.setId(Uuid.get());
    }
    return apiObject;
  }

}
