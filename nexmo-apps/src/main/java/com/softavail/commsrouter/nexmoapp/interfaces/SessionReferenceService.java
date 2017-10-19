package com.softavail.commsrouter.nexmoapp.interfaces;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.nexmoapp.domain.Session;
import com.softavail.commsrouter.nexmoapp.domain.SessionReference;
import com.softavail.commsrouter.nexmoapp.domain.SessionReferenceKey;

/**
 * Created by @author mapuo on 12.10.17.
 */
public interface SessionReferenceService {

  void create(SessionReference reference) throws CommsRouterException;

  Session getSessionByKey(SessionReferenceKey key) throws CommsRouterException;

}
