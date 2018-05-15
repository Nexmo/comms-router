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

import static org.apache.commons.collections.MapUtils.isNotEmpty;
import static org.pac4j.core.util.CommonHelper.assertNotNull;
import static org.pac4j.core.util.CommonHelper.assertTrue;

import io.buji.pac4j.profile.ShiroProfileManager;
import org.joda.time.DateTime;
import org.pac4j.core.client.Client;
import org.pac4j.core.client.Clients;
import org.pac4j.core.client.IndirectClient;
import org.pac4j.core.config.Config;
import org.pac4j.core.context.WebContext;
import org.pac4j.core.credentials.Credentials;
import org.pac4j.core.engine.DefaultCallbackLogic;
import org.pac4j.core.exception.HttpAction;
import org.pac4j.core.http.HttpActionAdapter;
import org.pac4j.core.profile.CommonProfile;
import org.pac4j.jwt.config.signature.SecretSignatureConfiguration;
import org.pac4j.jwt.profile.JwtGenerator;

import java.util.ArrayList;
import java.util.Map;

/**
 * @author Ergyun Syuleyman
 */
public class CommsRouterCallbackLogic<R, C extends WebContext> extends DefaultCallbackLogic<R, C> {

  private JwtGenerator jwtGenerator;

  private static final String PROFILE_ATTR_KEY_FIRSTNAME = "firstName";
  private static final String PROFILE_ATTR_KEY_LASTNAME = "lastName";
  private static final String PROFILE_ATTR_KEY_EXP_TIME = "notOnOrAfter";
  private static final String TOKEN_ATTR_KEY_NAME = "name";
  private static final String TOKEN_ATTR_KEY_EXPIRATION = "expiration";

  private static final String JWT_SALT = "12345678901234567890123456789012";

  public CommsRouterCallbackLogic() {
    super();
    this.setProfileManagerFactory(ShiroProfileManager::new);
    jwtGenerator = new JwtGenerator(new SecretSignatureConfiguration(JWT_SALT));
  }

  @SuppressWarnings("unchecked")
  @Override
  public R perform(
      final C context,
      final Config config,
      final HttpActionAdapter<R, C> httpActionAdapter,
      final String inputDefaultUrl,
      final Boolean inputMultiProfile,
      final Boolean inputRenewSession) {

    logger.debug("=== CALLBACK ===");

    final boolean multiProfile;
    if (inputMultiProfile == null) {
      multiProfile = false;
    } else {
      multiProfile = inputMultiProfile;
    }
    final boolean renewSession;
    if (inputRenewSession == null) {
      renewSession = true;
    } else {
      renewSession = inputRenewSession;
    }

    // checks
    assertNotNull("context", context);
    assertNotNull("config", config);
    assertNotNull("httpActionAdapter", httpActionAdapter);
    final Clients clients = config.getClients();
    assertNotNull("clients", clients);

    // logic
    final Client client = clients.findClient(context);
    logger.debug("client: {}", client);
    assertNotNull("client", client);
    assertTrue(client instanceof IndirectClient,
        "only indirect clients are allowed on the callback url");

    HttpAction action;
    try {
      final Map<String, String[]> requestParams = context.getRequestParameters();
      if (isNotEmpty(requestParams)) {
        logger.debug("requestParams: {}", requestParams);
      }

      final Credentials credentials = client.getCredentials(context);
      logger.debug("credentials: {}", credentials);

      final CommonProfile profile = client.getUserProfile(credentials, context);
      logger.debug("profile: {}", profile);
      saveUserProfile(context, config, profile, multiProfile, renewSession);
      String cookie = context.getSessionStore().getOrCreateSessionId(context);
      String formatedContent = String.format(
          ShiroConstants.AUTH_HTTP_CONTENT_FORMAT,
          getJwt(profile), cookie);
      action = respondWithCredentials(context, formatedContent);

    } catch (final HttpAction e) {
      logger.debug("extra HTTP action required in callback: {}", e.getCode());
      action = e;
    }

    return httpActionAdapter.adapt(action.getCode(), context);
  }

  protected HttpAction respondWithCredentials(final C context, final String inputContent) {
    logger.debug("respond OK content: {}", inputContent);
    return HttpAction.ok("OK", context, inputContent);
  }

  @SuppressWarnings("unchecked")
  protected String getJwt(CommonProfile profile) {
    String token = "";
    if (profile != null) {
      Object firstName = profile.getAttribute(PROFILE_ATTR_KEY_FIRSTNAME);

      String name = "";
      if (firstName != null) {
        if (firstName instanceof ArrayList
            && ((ArrayList) firstName).size() > 0) {
          name += ((ArrayList) firstName).get(0);
        } else if (firstName instanceof String) {
          name += firstName;
        }
      }

      Object lastName = profile.getAttribute(PROFILE_ATTR_KEY_LASTNAME);

      if (lastName != null) {
        if (lastName instanceof ArrayList
            && ((ArrayList) lastName).size() > 0) {
          if (!name.isEmpty()) {
            name += " ";
          }
          name += ((ArrayList) lastName).get(0);
        } else if (lastName instanceof String) {
          if (!name.isEmpty()) {
            name += " ";
          }
          name += lastName;
        }
      }
      if (!name.isEmpty()) {
        profile.removeAttribute(TOKEN_ATTR_KEY_NAME);
        profile.addAttribute(TOKEN_ATTR_KEY_NAME, name);
      }

      Object dt = profile.getAttribute(PROFILE_ATTR_KEY_EXP_TIME);

      if (dt != null) {
        profile.removeAttribute(TOKEN_ATTR_KEY_EXPIRATION);
        long expiration = ((DateTime) dt).getMillis();
        profile.addAttribute(TOKEN_ATTR_KEY_EXPIRATION, expiration);
      }

      token = jwtGenerator.generate(profile);
    }
    return token;
  }

}
