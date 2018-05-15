/*
 * Copyright 2018 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.softavail.commsrouter.shiro;

import static org.pac4j.core.util.CommonHelper.assertNotNull;
import static org.pac4j.core.util.CommonHelper.isEmpty;
import static org.pac4j.core.util.CommonHelper.isNotEmpty;

import io.buji.pac4j.profile.ShiroProfileManager;
import org.apache.shiro.SecurityUtils;
import org.apache.shiro.subject.Subject;
import org.pac4j.core.client.Client;
import org.pac4j.core.client.Clients;
import org.pac4j.core.client.DirectClient;
import org.pac4j.core.config.Config;
import org.pac4j.core.context.WebContext;
import org.pac4j.core.credentials.Credentials;
import org.pac4j.core.engine.DefaultSecurityLogic;
import org.pac4j.core.engine.SecurityGrantedAccessAdapter;
import org.pac4j.core.exception.HttpAction;
import org.pac4j.core.exception.TechnicalException;
import org.pac4j.core.http.HttpActionAdapter;
import org.pac4j.core.profile.CommonProfile;
import org.pac4j.core.profile.ProfileManager;

import java.util.List;

/**
 * @author Ergyun Syuleyman
 */
public class CommsRouterSecurityLogic<R, C extends WebContext> extends DefaultSecurityLogic<R, C> {

  private Boolean forceAuthorize;


  public CommsRouterSecurityLogic() {
    super();
    this.setProfileManagerFactory(ShiroProfileManager::new);
    forceAuthorize = false;
  }


  @SuppressWarnings("unchecked")
  @Override
  public R perform(
      final C context,
      final Config config,
      final SecurityGrantedAccessAdapter<R, C> securityGrantedAccessAdapter,
      final HttpActionAdapter<R, C> httpActionAdapter,
      final String clients,
      final String authorizers,
      final String matchers,
      final Boolean inputMultiProfile,
      final Object... parameters) {

    logger.debug("=== SECURITY ===");

    // default value
    final boolean multiProfile;
    if (inputMultiProfile == null) {
      multiProfile = false;
    } else {
      multiProfile = inputMultiProfile;
    }

    // checks
    assertNotNull("context", context);
    assertNotNull("config", config);
    assertNotNull("httpActionAdapter", httpActionAdapter);
    assertNotNull("clientFinder", getClientFinder());
    assertNotNull("authorizationChecker", getAuthorizationChecker());
    assertNotNull("matchingChecker", getMatchingChecker());
    final Clients configClients = config.getClients();
    assertNotNull("configClients", configClients);

    // logic
    HttpAction action;
    try {

      logger.debug("url: {}", context.getFullRequestURL());
      logger.debug("matchers: {}", matchers);
      if (getMatchingChecker().matches(context, matchers, config.getMatchers())) {

        logger.debug("clients: {}", clients);
        final List<Client> currentClients = getClientFinder().find(configClients, context, clients);
        logger.debug("currentClients: {}", currentClients);

        final boolean loadProfilesFromSession = loadProfilesFromSession(context, currentClients);
        logger.debug("loadProfilesFromSession: {}", loadProfilesFromSession);
        final ProfileManager<CommonProfile> manager = getProfileManager(context, config);
        List<CommonProfile> profiles = manager.getAll(loadProfilesFromSession);
        logger.debug("profiles: {}", profiles);

        // no profile and some current clients
        if (isEmpty(profiles) && isNotEmpty(currentClients)) {
          boolean updated = false;
          // loop on all clients searching direct ones to perform authentication
          for (final Client<Credentials, CommonProfile> currentClient : currentClients) {
            if (currentClient instanceof DirectClient) {
              logger.debug("Performing authentication for direct client: {}", currentClient);

              final Credentials credentials = currentClient.getCredentials(context);
              logger.debug("credentials: {}", credentials);
              final CommonProfile profile = currentClient.getUserProfile(credentials, context);
              logger.debug("profile: {}", profile);
              if (profile != null) {
                final boolean saveProfileInSession = saveProfileInSession(
                    context, currentClients, (DirectClient) currentClient, profile);
                logger.debug("saveProfileInSession: {} / multiProfile: {}",
                    saveProfileInSession, multiProfile);
                manager.save(saveProfileInSession, profile, multiProfile);
                updated = true;
                if (!multiProfile) {
                  break;
                }
              }
            }
          }
          if (updated) {
            profiles = manager.getAll(loadProfilesFromSession);
            logger.debug("new profiles: {}", profiles);
          }
        }

        // we have profile(s) -> check authorizations
        if (isNotEmpty(profiles) && !forceAuthorize) {
          logger.debug("authorizers: {}", authorizers);
          if (getAuthorizationChecker().isAuthorized(context, profiles, authorizers,
              config.getAuthorizers())) {
            logger.debug("authenticated and authorized -> grant access");
            return securityGrantedAccessAdapter.adapt(context, parameters);
          } else {
            logger.debug("forbidden");
            action = forbidden(context, currentClients, profiles, authorizers);
          }
        } else {
          if (startAuthentication(context, currentClients)) {
            logger.debug("Starting authentication");
            saveRequestedUrl(context, currentClients);
            if (forceAuthorize) {
              manager.logout();
              Subject subject = SecurityUtils.getSubject();
              subject.logout();
            }
            action = redirectToIdentityProvider(context, currentClients);
          } else {
            logger.debug("unauthorized");
            action = unauthorized(context, currentClients);
          }
        }

      } else {

        logger.debug("no matching for this request -> grant access");
        return securityGrantedAccessAdapter.adapt(context, parameters);
      }

    } catch (final HttpAction e) {
      logger.debug("extra HTTP action required in security: {}", e.getCode());
      action = e;
    } catch (final TechnicalException e) {
      throw e;
    } catch (final Throwable e) {
      throw new TechnicalException(e);
    }

    return httpActionAdapter.adapt(action.getCode(), context);
  }


  public Boolean getForceAuthorize() {
    return forceAuthorize;
  }

  public void setForceAuthorize(final Boolean forceAuthorize) {
    this.forceAuthorize = forceAuthorize;
  }

}
