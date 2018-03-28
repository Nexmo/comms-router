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

import io.buji.pac4j.profile.ShiroProfileManager;
import org.pac4j.core.client.Client;
import org.pac4j.core.client.Clients;
import org.pac4j.core.client.IndirectClient;
import org.pac4j.core.config.Config;
import org.pac4j.core.context.Pac4jConstants;
import org.pac4j.core.context.WebContext;
import org.pac4j.core.credentials.Credentials;
import org.pac4j.core.engine.DefaultCallbackLogic;
import org.pac4j.core.exception.HttpAction;
import org.pac4j.core.http.HttpActionAdapter;
import org.pac4j.core.profile.CommonProfile;
import static org.pac4j.core.util.CommonHelper.assertNotNull;
import static org.pac4j.core.util.CommonHelper.assertTrue;
import static org.pac4j.core.util.CommonHelper.isNotBlank;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Ergyun Syuleyman
 */
public class CommsRouterCallbackLogic<R, C extends WebContext>  extends DefaultCallbackLogic<R, C> {

    private static String content = "<!DOCTYPE html>\n" +
"<html>\n" +
"<head>\n" +
"<title>Authentication</title>\n" +
"<meta charset=\"UTF-8\">\n" +
"<meta content=\"width=device-width, initial-scale=1.0\" name=\"viewport\">\n" +
"</head>\n" +
"<body>\n" +
"<div></div>\n" +
"<script>\n" +
"    var search = location.search;\n" +
"    if (window.opener) {\n" +
"        var message = {};\n" + "        message.type = \"sso-message\";\n" + "        "
      + "message.userName = \"userName\";\n"
      + "        message.token = \"token\";\n"
      + "        message.sessionExpireTime = \"sessionExpireTime\";\n"
      + "        //check for IE11\n" +
"        if (!(window.ActiveXObject) && \"ActiveXObject\" in window) {\n" +
"            window.opener.handleSsoAuthentication(message);\n" +
"            window.close();\n" +
"        } else {\n" +
"            window.opener.postMessage(message, '*');\n" +
"            window.close();\n" +
"        }\n" +
"    }     \n" +
"</script>\n" +
"</body>\n" +
"</html>";
    public CommsRouterCallbackLogic() {
        super();
        this.setProfileManagerFactory(ShiroProfileManager::new);
    }
    

    protected Logger logger = LoggerFactory.getLogger(getClass());

    @Override
    public R perform(final C context, final Config config, final HttpActionAdapter<R, C> httpActionAdapter,
                     final String inputDefaultUrl, final Boolean inputMultiProfile, final Boolean inputRenewSession) {

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
        assertTrue(client instanceof IndirectClient, "only indirect clients are allowed on the callback url");

        HttpAction action;
        try {
            final Credentials credentials = client.getCredentials(context);
            logger.debug("credentials: {}", credentials);

            final CommonProfile profile = client.getUserProfile(credentials, context);
            logger.debug("profile: {}", profile);
            saveUserProfile(context, config, profile, multiProfile, renewSession);
            action = respondWithCredentials(context, content);

        } catch (final HttpAction e) {
            logger.debug("extra HTTP action required in callback: {}", e.getCode());
            action = e;
        }

        return httpActionAdapter.adapt(action.getCode(), context);
    }

        protected HttpAction respondWithCredentials(final C context, final String inputContent) {
        final String requestedUrl = (String) context.getSessionAttribute(Pac4jConstants.REQUESTED_URL);
        if (isNotBlank(requestedUrl)) {
            context.setSessionAttribute(Pac4jConstants.REQUESTED_URL, null);
        }
        logger.debug("respond OK content: {}", inputContent);
        return HttpAction.ok("OK", context, inputContent);
    }

}
