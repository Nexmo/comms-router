package com.softavail.commsrouter.nexmoapp.interfaces;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.nexmoapp.plugin.Plugin;

/**
 * Created by @author mapuo on 12.10.17.
 */
public interface PluginService {

  Plugin findByName(String name) throws CommsRouterException;

}
