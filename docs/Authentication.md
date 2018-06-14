# Apache Shiro
CommsRouter protects its REST APIs through the ApacheShiro Java security framework.

Apache Shiro™ is a powerful and easy-to-use Java security framework that performs authentication, authorization, cryptography, and session management.


# Authentication scheme
CommsRouter does not implement a built-in authentication scheme. Instead it provides authentication to its users via SAML.

# What Is SAML?
Security Assertion Markup Language (SAML) is an XML-based framework for authentication and authorization between two entities: a Service Provider and an Identity Provider. The Service Provider agrees to trust the Identity Provider to authenticate users. In return, the Identity provider generates an authentication assertion, which indicates that a user has been authenticated.

SAML is a standard single sign-on (SSO) format. Authentication information is exchanged through digitally signed XML documents. It's a complex single sign-on (SSO) implementation that enables seamless authentication, mostly between businesses and enterprises.

With SAML, you don't have to worry about typing in authentication credentials or remembering and resetting passwords.

# How SAML Works

SAML SSO works by transferring the user’s identity from one place (the identity provider) to the CommsRouter (the service provider). This is done through an exchange of digitally signed XML documents.

Consider the following scenario: A user is logged into a system that acts as an identity provider. The user wants to log in to a CommsRouter (the service provider). The following happens:

1. The user accesses the CommsRouter application using a link on an intranet, a bookmark, or similar and the application loads.

1. The CommsRouter identifies the user’s origin (by application subdomain, user IP address, or similar) and redirects the user back to the identity provider, asking for authentication. This is the authentication request.

1. The user either has an existing active browser session with the identity provider or establishes one by logging into the identity provider.

1. The identity provider builds the authentication response in the form of an XML-document containing the user’s username or email address, signs it using an X.509 certificate, and posts this information to the CommsRouter.

1. The CommsRouter, which already knows the identity provider and has a certificate fingerprint, retrieves the authentication response and validates it using the certificate fingerprint.

1. The identity of the user is established and the user is provided with app access.

# ComsmRouter SAML Configuration

As CommsRouter uses Apache Shiro, the configuration must be defined in your `shiro.ini` file as follow:

```ini
[main]
# Saml authentication settings
saml2Config = org.pac4j.saml.client.SAML2ClientConfiguration
saml2Config.keystorePath = [samlKeystore.jks]
saml2Config.keystorePassword = [keystore-passwd]
saml2Config.privateKeyPassword = [priv-key-passwd]
saml2Config.identityProviderMetadataPath = [path_to_idp_specific_metadata_xml_file]
saml2Config.serviceProviderEntityId = [sp_entity_id]
saml2Config.serviceProviderMetadataPath = [path_to_sp_specific_metadata_xml_file]
saml2Config.maximumAuthenticationLifetime = 86400

saml2Client = org.pac4j.saml.client.SAML2Client
saml2Client.configuration = $saml2Config

# general settings
securityManager = org.apache.shiro.web.mgt.DefaultWebSecurityManager
pac4jSubjectFactory = io.buji.pac4j.subject.Pac4jSubjectFactory
securityManager.subjectFactory = $pac4jSubjectFactory
sessionManager = org.apache.shiro.web.session.mgt.DefaultWebSessionManager
cacheManager = org.apache.shiro.cache.MemoryConstrainedCacheManager
securityManager.cacheManager = $cacheManager
securityManager.sessionManager = $sessionManager
securityManager.sessionManager.globalSessionTimeout = 86400000
securityManager.sessionManager.sessionValidationInterval=86400000

# clients settings
clients.callbackUrl = [idp_redirect_client_to_this_commsrouter_callback_url]
# e.g https://commsrouter-serverr-address:port/subpath/callback?client_name=SAML2Client
# note the url must end with callback?client_name=SAML2Client
clients.clients = $saml2Client

securityLogic = com.softavail.commsrouter.shiro.CommsRouterSecurityLogic
securityLogic.forceAuthorize = true
saml2SecurityFilter = io.buji.pac4j.filter.SecurityFilter
saml2SecurityFilter.config = $config
saml2SecurityFilter.securityLogic = $securityLogic
saml2SecurityFilter.clients = SAML2Client

logout = io.buji.pac4j.filter.LogoutFilter
logout.config = $config
logout.localLogout = false
logout.centralLogout = true
logout.logoutUrlPattern = /.*

logoutCallbackLogic = com.softavail.commsrouter.shiro.CommsRouterLogoutCallbackLogic
logoutCallback = io.buji.pac4j.filter.LogoutFilter
logoutCallback.config = $config
logoutCallback.logoutLogic = $logoutCallbackLogic

authFilter = com.softavail.commsrouter.shiro.CommsRouterAuthFilter

callbackLogic = com.softavail.commsrouter.shiro.CommsRouterCallbackLogic
callbackFilter = io.buji.pac4j.filter.CallbackFilter
callbackFilter.config = $config
callbackFilter.callbackLogic = $callbackLogic

[urls]
/login = saml2SecurityFilter
/callback = callbackFilter
/logout = logout
/logoutCallback = logoutCallback
/api/** = authFilter
```
* Note: items in ```shiro.ini``` enclosed with ```[]``` are specific with the comms-router deployemnt and must be defined by the deployer of the comms router service.

# No authentication
In order to turn off authentication, the minimal shiro.ini file should be:
```ini
[main]

noAuthFilter = com.softavail.commsrouter.shiro.CommsRouterNoAuthFilter

[urls]
/login = noAuthFilter
/** = anon

```

# Deployment Configuration

In the deployment configuration, `shiro.ini` file must be specified in the ```application.properties``` file as follow:

```
shiro.configLocations=/opt/local/tomcat/conf/comms-router-web/shiro.ini
```
