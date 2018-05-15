/*
 * Copyright 2018 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.commsrouter.shiro;

import static org.pac4j.core.util.CommonHelper.assertNotNull;

import io.buji.pac4j.profile.ShiroProfileManager;
import org.pac4j.core.client.Clients;
import org.pac4j.core.config.Config;
import org.pac4j.core.context.WebContext;
import org.pac4j.core.context.session.SessionStore;
import org.pac4j.core.engine.DefaultLogoutLogic;
import org.pac4j.core.exception.HttpAction;
import org.pac4j.core.http.HttpActionAdapter;
import org.pac4j.core.profile.CommonProfile;
import org.pac4j.core.profile.ProfileManager;

import java.util.List;

/**
 * @author Ergyun Syuleyman
 */
public class CommsRouterLogoutCallbackLogic<R, C extends WebContext>
    extends DefaultLogoutLogic<R, C> {

  public CommsRouterLogoutCallbackLogic() {
    super();
    this.setProfileManagerFactory(ShiroProfileManager::new);
  }

  @SuppressWarnings("unchecked")
  @Override
  public R perform(
      final C context,
      final Config config,
      final HttpActionAdapter<R, C> httpActionAdapter,
      final String defaultUrl,
      final String inputLogoutUrlPattern,
      final Boolean inputLocalLogout,
      final Boolean inputDestroySession,
      final Boolean inputCentralLogout) {
    logger.debug("=== LOGOUT CALLBACK ===");

    // checks
    assertNotNull("context", context);
    assertNotNull("config", config);
    assertNotNull("httpActionAdapter", httpActionAdapter);
    final Clients configClients = config.getClients();
    assertNotNull("configClients", configClients);

    // logic
    final ProfileManager manager = getProfileManager(context, config);
    final List<CommonProfile> profiles = manager.getAll(true);
    HttpAction action = HttpAction.ok("ok", context);

    // local logout if requested or multiple profiles
    if (profiles.size() > 0) {
      logger.debug("Performing application logout");
      manager.logout();
      final SessionStore sessionStore = context.getSessionStore();
      if (sessionStore != null) {
        final boolean removed = sessionStore.destroySession(context);
        if (!removed) {
          logger.error(
              "Unable to destroy the web session. The session store may not support this feature");
        }
      } else {
        logger.error("No session store available for this web context");
      }
    }

    return httpActionAdapter.adapt(action.getCode(), context);
  }

}

