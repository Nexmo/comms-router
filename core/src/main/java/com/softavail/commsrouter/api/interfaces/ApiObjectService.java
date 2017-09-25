package com.softavail.commsrouter.api.interfaces;

import com.softavail.commsrouter.api.exception.CommsRouterException;

import java.util.List;

/**
 * Created by @author mapuo on 04.09.17.
 */
public interface ApiObjectService<ENTITYT> {

  ENTITYT get(String id) throws CommsRouterException;

  List<ENTITYT> list() throws CommsRouterException;

  void delete(String id) throws CommsRouterException;

}
