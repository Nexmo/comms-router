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

import org.apache.shiro.web.filter.authc.AuthenticationFilter;
import org.apache.shiro.web.util.WebUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

/**
 *
 * @author Ergyun Syuleyman
 */
public class CommsRouterNoAuthFilter extends AuthenticationFilter {

  protected Logger logger = LoggerFactory.getLogger(getClass());

  @Override
  protected boolean isAccessAllowed(
      ServletRequest request, ServletResponse response, Object mappedValue) {
    boolean allowed = false;

    logger.debug("isAccessAllowed: {}", allowed);
    return allowed;
  }

  @Override
  protected boolean onAccessDenied(ServletRequest request, ServletResponse response)
      throws Exception {

    boolean accessDenied = false;
    logger.debug("onAccessDenied: {}", accessDenied);
    return accessDenied;
  }
  
  private String generateContent() {
    return String.format(ShiroConstants.AUTH_HTTP_CONTENT_FORMAT,
        ShiroConstants.NO_AUTH_JWT, "always-access-granted-token");
  }

  @Override
  public void afterCompletion(ServletRequest request, ServletResponse response, Exception exception)
      throws Exception {

    logger.debug("afterCompletion");
    
    HttpServletResponse httpResponse = WebUtils.toHttp(response);
    httpResponse.setContentType("text/html");
    httpResponse.setStatus(200);
    httpResponse.getWriter().write(generateContent());
  }

}
