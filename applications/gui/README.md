# Introduction to Comms Router User Interface
The Nexmo Comms Router API enables businesses to leverage self-hosted or on premises APIs to manage Contact Center task management. The modern Contact Center is designed to be flexible and extensible through allowing developers to customise integrations and leverage Nexmo Comms Router API as a key building block.

Comms Router enables routing tasks coming in through a Voice application to be routed to a live agent dependent on skills or specific critiera. In order to manage the Agents, Skills and Queues within general operations of a Contact Center having access to a web interface to do this is an important feature.

Comms Router user interface allows Admins to do the following:
* Set the status of Agents (Online, Offline)
* Manage Agent skillsets
* Manage critieria for routing customers through specific Queues 
* View incoming Tasks
* View Agents availabilty

## Features
* Near real-time monitoring of Contact Center activities (Agents and Tasks)
* Easy integratio into SAML or OAuth provider
* Enables operational Admins to manage Agents (skills and availability) and Queues

## Pre-requisites
* Nexmo Account https://dashboard.nexmo.com/sign-up
* Nexmo Phone Numbers https://developer.nexmo.com/account/guides/numbers
* Nexmo Voice API https://developer.nexmo.com/voice/voice-api/overview
* Nexmo [Comms Router API installed](tree/master/)

## Installation
... @Mario / @Illa

### Setting up access control
SAML / OAuth configration

it should be disabled only on backend
in application.properties - shiro.configLocations=file:/opt/tomcat/conf/comms-router-web/noauth-shiro.ini
and noauth-shiro.ini is:
cat noauth-shiro.ini 
[main]
############################################################################
# CONFIG:
############################################################################

noAuthFilter = com.softavail.commsrouter.shiro.CommsRouterNoAuthFilter

[urls]
/login = noAuthFilter
/** = anon



## How to use
The Comms Router GUI enables Adminstrators to manage Contact Center Agents and workflows, the following documentation is to help Adminstrators get to grips with the user interface.

### Logging in
Once the authentician methodology has been implemented and users added to which ever 3rd party access control management system they can log into the Comms Router user interface.

1. Go to the homepage (e.g. http://localhost/comms-router-gui/#/)
2. Click the `Log in` button
3. Enter username and password and signin

![...](comms_router_gui_logging_in.png)

### Manageing Agent Skills

![...](comms_router_gui_agent_skills.png)

### Managing Queues
![...](comms_router_gui_queues.png)

### Managing Plans
![...](comms_router_gui_plans.png)

### Managing Agents availabilty
![...](comms_router_gui_agents_availability.png)

### Viewing Tasks
![...](comms_router_gui_viewing_tasks.png)


## Supporting technical documentation
* [Getting Started Guide](docs/GettingStartedGuide.md) for quick start.
* Predicate [expression guide](docs/ExpressionSyntax.md) for Agents and Queues.
* Set up of [database access and Tomcat configuration](docs/ConfiguringDatabaseAccess.md).
* How to [manage database migrations](docs/ManageDBMigrations.md).
* OpenAPI spec on localhost http://localhost:8080/comms-router-web/swagger-ui.html
