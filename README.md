# Comms Router

## Overview

Comms Router implements routing tasks to agents for handling. Routing is based on skills needed for the tasks and capabilities of the agents.

### Concepts

  * Task - work item characterized by its requirements - set of skills needed expressed by the user as key/value pairs.
  * Agent - abstract entity able to handle tasks, characterized by its capabilities - the skills it has expressed by the user as key/value pairs.
  * Queue - container for tasks waiting for the next available agent. It has a predicated defined by the user that is matched agains the agents' skills to select these able to serve the queue.
  * Plan - defines how a task is handled by the router selecting a queue for it
  * Router - a container for the router entities allowing different user application to share the same database

### How it works

User application creates a router and then one or more queues, agents and plans.

Then the application calls the router to creates a task with its requirements and either places it in a queue or assigns a plan to it.

The plan contains list of rules, which the router executes in order. First rule which predicate matches the task requirements determines the queue for this task.

When an agent become available for this task, it is put into state "busy" and the callback URL provided by the user for this task is called with information about the task and the selected agent.

The user's application then informs the agent about the task and tracks the task's completion. When the task is done, the user's application changes the task's state within the router, which makes the agent available for subsequent tasks.

## The Router

### Requirements

  * Java - Oracle JDK/JRE 8 (build/runtime)
  * Apache Maven - 3.5 (build)
  * SQL Server - MySQL 5.7 (runtime)
  * Java Servlet Container - Tomcat 8 (runtime)

Although the software may work with different flavors of Java, SQL Server or Web Container, currently it is being tested with these listed above.

### Build

Install Java and Maven, clone the repo and execute:

`mvn install`

The resulting war file should be:

`web/target/comms-router-web.war`

### Install

Create database for the router.

Create DB user to be used by the router and grant this user access to the newly created database.

Configure JNDI data source with name "jdbc/commsRouterDB".
Details on how to do this in Tomcat can be found [here](web/README.md).

Deploy comms-router-web.war into Tomcat.
Depending on your Tomcat settings this can be done by simple copying it to the Tomcat's webapps directory.

### Test

List routers:

`curl http://localhost:8080/comms-router-web/api/routers`

## Demo application

The purpose of the Demo application is to demonstrate how to
integrate the Nexmo API with the CommsRouter.

The demo application exposes a public REST API (WebHooks),
which Nexmo can call into, in order to notify us when a new incoming call arrives.

When a new WebHook call arrives to the Demo app, it creates a Task in the CommsRouter.
The task contains a WebHook where the CommsRouter will callback us when an agent is found.
When the CommsRouter calls the Task's WebHook, the Demo app then takes the agent's address and
creates a voice call to the agent. Then the two call legs are put in a dedicated Nexmo conversation.
When the agent's call leg ends, the task is marked as completed in the CommsRouter.

### Prerequisites
In order to use the demo application you'll need to:

* Create a [Nexmo account](https://dashboard.nexmo.com/sign-up)
* Create a [Nexmo Voice Application](https://dashboard.nexmo.com/voice/create-application)
* [Buy](https://dashboard.nexmo.com/buy-numbers) one number from Nexmo and
  associate it with the Nexmo Voice Application
> [Here](https://developer.nexmo.com/tutorials/add-a-call-whisper-to-an-inbound-call#create-a-voice-application)
  is a tutorial where you can see how to:
> * Create a Voice Application
> * Buy a Phone Number
> * Link the Phone Number to a Nexmo Application

### Configuration

Before installing the demo app, you'll need to make some configuration changes.

Find application.properties file which has the following structure:
```
app.callbackBaseUrl=
app.phone=
app.musicOnHoldUrl=
comms.routerUrl=
comms.routerId=
nexmo.appId=
nexmo.appPrivateKey=
```
Please put the values inside the application properties as follow:

* __app.callbackBaseUrl__ Base URL to the server where the demo app is
  served from. For example http://host:port/demo-app-root/api

* __app.phone__ This is the number you bought from Nexmo and associated it
  with your voice application

* __app.musicOnHoldUrl__ This is an URL to the mp3 file to stream to the customer until an available Agent is found.

* __comms.routerUrl__ This the base URL to the CommsRouter REST API.
  For example  http://commsrouterhost:port/comms-router-web-api/api

* __comms.routerId__ This is the id of the router object in the CommsRouter

* __nexmo.appId__ This is the app-id from your Nexmo Voice application

* __nexmo.appPrivateKey__ This is the filename (PEM file) with the private key
  from your Nexmo voice application. This PEM file must be in the same directory
  where _application.properties_ file is.

Then add a system property of the JVM with key _comms.demo.app.config.path_ that
will tell the app where to find the _application.properties_ file.

Ex. `java -Dcomms.demo.app.config.path=/configDir`.
Ex. Tomcat has `bin/setenv.sh` where you can say
```bash
export CATALINA_OPTS="$CATALINA_OPTS -Dcomms.demo.app.config.path=/configDir"
```

### Initialize CommsRouter

Before using the demo app, you'll need to create a router object, a Queue and an Agent
in the CommsRouter via its REST API.

Router object must be with an ID equal to the id specified in the *comms.routerId* parameter
in _application.properties_ file.

A Queue object must be with an id set to _queue-demo_.

An Agent must have a valid address (phone number) where the Nexmo will make a voice call.
