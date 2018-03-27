/* 
 * Copyright 2017 SoftAvail Inc.
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

package com.softavail.commsrouter.webservice;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.webservice.config.ManifestConfigurationImpl;
import io.swagger.jaxrs.config.BeanConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.glassfish.jersey.server.ResourceConfig;

import javax.servlet.ServletContext;
import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Context;

/**
 * Created by @author mapuo on 31.08.17.
 */
@ApplicationPath("/api")
public class CommsRouterApplication extends ResourceConfig {

  private static final Logger LOGGER = LogManager.getLogger(CommsRouterApplication.class);

  public CommsRouterApplication(@Context ServletContext servletContext)
      throws CommsRouterException {

    ApplicationContext applicationContext =
        (ApplicationContext) servletContext.getAttribute(WebServletListener.APPLICATION_CONTEXT);

    ManifestConfigurationImpl manifest = applicationContext.getManifest();
    LOGGER.info("Build info for {}; Version: {}-{}; Build at {}; Build on JDK {}",
        manifest.getImplementationTitle(),
        manifest.getImplementationVersion(),
        manifest.getImplementationBuild(),
        manifest.getImplementationBuildTime(),
        manifest.getBuildJdk());

    register(new ApplicationBindings(applicationContext.getCoreContext()));

    packages(CommsRouterApplication.class.getPackage().getName());

    register(io.swagger.jaxrs.listing.ApiListingResource.class);
    register(io.swagger.jaxrs.listing.SwaggerSerializers.class);

    BeanConfig beanConfig = new BeanConfig();
    beanConfig.setSchemes(new String[] {"https","http"});
    beanConfig.setBasePath(servletContext.getContextPath() + "/api");
    beanConfig.setResourcePackage("com.softavail.commsrouter.webservice.resources");
    beanConfig.setScan(true);
    beanConfig.setPrettyPrint(true);

    LOGGER.debug("Application started!");
  }

}
