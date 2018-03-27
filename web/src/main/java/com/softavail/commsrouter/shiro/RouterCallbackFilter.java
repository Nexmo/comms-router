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

import io.buji.pac4j.context.ShiroSessionStore;
import io.buji.pac4j.filter.CallbackFilter;
import java.io.IOException;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.pac4j.core.context.J2EContext;
import org.pac4j.core.context.session.SessionStore;
import org.pac4j.core.http.HttpActionAdapter;
import org.pac4j.core.http.J2ENopHttpActionAdapter;
import static org.pac4j.core.util.CommonHelper.assertNotNull;

/**
 *
 * @author Ergyun Syuleyman
 */
public class RouterCallbackFilter extends CallbackFilter {

  @Override
  public void doFilter(final ServletRequest servletRequest, final ServletResponse servletResponse,
      final FilterChain filterChain) throws IOException, ServletException {

    assertNotNull("callbackLogic", getCallbackLogic());
    assertNotNull("config", getConfig());

    final HttpServletRequest request = (HttpServletRequest) servletRequest;
    final HttpServletResponse response = (HttpServletResponse) servletResponse;
    final SessionStore<J2EContext> sessionStore = getConfig().getSessionStore();
    final J2EContext context = new J2EContext(request, response,
        sessionStore != null ? sessionStore : ShiroSessionStore.INSTANCE);
    final HttpActionAdapter<Object, J2EContext> adapter =
        getHttpActionAdapter() != null ? getHttpActionAdapter() : J2ENopHttpActionAdapter.INSTANCE;

    getCallbackLogic().perform(context, getConfig(), adapter, this.getDefaultUrl(),
        this.getMultiProfile(), false);
  }

}
