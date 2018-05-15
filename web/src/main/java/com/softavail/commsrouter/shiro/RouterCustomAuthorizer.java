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

import org.pac4j.core.authorization.authorizer.ProfileAuthorizer;
import org.pac4j.core.context.WebContext;
import org.pac4j.core.exception.HttpAction;
import org.pac4j.core.profile.CommonProfile;

import java.util.List;

/**
 * @author Ergyun Syuleyman
 */
public final class RouterCustomAuthorizer extends ProfileAuthorizer<CommonProfile> {

  @Override
  public boolean isAuthorized(final WebContext context, final List<CommonProfile> profiles)
      throws HttpAction {

    return isAnyAuthorized(context, profiles);
  }

  @Override
  public boolean isProfileAuthorized(final WebContext context, final CommonProfile profile) {
    if (profile == null) {
      return false;
    }
    return (!profile.getUsername().isEmpty() && profile.getAttribute("roles") != null);
  }
}
