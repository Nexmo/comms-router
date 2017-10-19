package com.softavail.commsrouter.webservice;

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

  public CommsRouterApplication(@Context ServletContext servletContext) {
    ApplicationContext applicationContext =
        (ApplicationContext) servletContext.getAttribute(WebServletListener.APPLICATION_CONTEXT);

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
