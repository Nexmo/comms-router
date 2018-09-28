# Demo application overview

The purpose of the demo application is to demonstrate how to integrate the Nexmo API with the Comms Router. The application can be used as in or customised to be used as required for your specific use case.

The demo application exposes a public REST API (webhooks), which Nexmo can call into, in order to notify us when a new incoming call arrives.

## How it works
When a new webhook call arrives to the demo application, it creates a Task in the Comms Router. The Task contains a webhook where the Comms Router will trigger a callback when an Agent is found.

The Comms Router calls the Task's webhook, the demo application then takes the agent's address and creates a voice call to the agent. Then the two call legs are put in a dedicated Nexmo conversation. When the Agent's call leg ends, the Task is marked as completed in the Comms Router.

## Prerequisites
* Setup a [Nexmo account](https://dashboard.nexmo.com/sign-up)
* Rent a virtual number using [Dashboard](https://dashboard.nexmo.com/buy-numbers) or [Developer API](https://developer.nexmo.com/api/developer/numbers) and set the webhook endpoint to your app
* Create a [Voice application](https://developer.nexmo.com/concepts/guides/applications#apps_quickstart) and set it up by adding a [Nexmo Call Control Object](), webhook for events and link the virtual number.

An overview and Getting Started Guide for Voice can be found here https://developer.nexmo.com/voice/voice-api/overview

## 1. Configuration
Before installing the demo application, you'll need to make some configuration changes to the Comms Router installation.

Find `db-migrations/src/main/resources/liquibase.properties` file which has the following structure:

```parameters
app.callbackBaseUrl=
app.nexmoCallbackBaseUrl=
app.phone=
app.musicOnHoldUrl=
comms.routerUrl=
comms.routerId=
comms.planId=
comms.queueId=
nexmo.appId=
nexmo.appPrivateKey=
```

Parameter | Description
-- | -- |
`app.nexmoCallbackBaseUrl` | This URL will be used from the Nexmo server for invoking web hooks, e.g. http://host:port/demo-app-root/api
`app.callbackBaseUrl` | This URL will be used from the Comms Router web app for invoking Task callback hooks, e.g. http://host:port/demo-app-root/api
`app.phone` | This is the number you bought from Nexmo and associated it with your voice application 
`app.musicOnHoldUrl` | This is an URL to the mp3 file to stream to the customer until an available Agent is found.
`comms.routerUrl` | This the base URL to the CommsRouter REST API. For example http://commsrouterhost:port/comms-router-web-api/api
`comms.routerId` | This is the Id of the router object in the Comms Router
`comms.planId` | This is the Id of the plan object in the Comms Router
`comms.queueId` | This is the Id of the default queue object in the Comms Router
`nexmo.appId` | This is the application Id from your Nexmo Voice application
`nexmo.appPrivateKey` | This is the filename (PEM file) with the private key from your Nexmo voice application. This PEM file must be in the same directory where `application.properties` file is.


## 2. Update system property
Add a system property of the JVM with key `comms.demo.app.config.path` that will tell the application where to find the `application.properties` file.

#### UNIX
Update your `setenv.sh` file `$CATALINA_BASE/bin/setenv.sh`.
```bash
export CATALINA_OPTS="$CATALINA_OPTS -Dcomms.demo.app.config.path=/configDir"
```

#### Windows
Update your `setenv.bat` file `%CATALINA_BASE%\bin\setenv.bat`.
```bat
set CATALINA_OPTS=%CATALINA_OPTS% -Dcomms.demo.app.config.path=c:\configDir
```

## 3. Initialize Comms Router
Before using the demo application, you'll need to create Router, Queue, Agent and Plan objects in the Comms Router via its REST API. Router, Queue, Plan objects, must be with an `Id` equal to the `Id` specified in the `comms.routerId` parameter in `application.properties` file.

An Agent must have a valid address (phone number) where the Nexmo will make a voice call.

## Supporting documentation
* [Getting Started Guide](docs/GettingStartedGuide.md) for quick start.
* Predicate [expression guide](docs/ExpressionSyntax.md) for Agents and Queues.
* Set up of [database access and Tomcat configuration](docs/ConfiguringDatabaseAccess.md).
* How to [manage database migrations](docs/ManageDBMigrations.md).
* OpenAPI spec on localhost http://localhost:8080/comms-router-web/swagger-ui.html
