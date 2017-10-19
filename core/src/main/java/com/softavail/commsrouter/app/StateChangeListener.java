package com.softavail.commsrouter.app;

import java.util.EventListener;

/**
 * Created by @author mapuo on 18.10.17.
 */
@FunctionalInterface
public interface StateChangeListener extends EventListener {

  void stateChanged(StateChangeEvent changeEvent);

}
