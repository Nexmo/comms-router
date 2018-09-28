# Quick Start Guide
This guide provides an overview of the main resources for the Comms Router API, introduces the prerequisites and assists with enabling first application build.

**Note:** the commands listed below are designed to be used with a Unix shell. Some of them need to be modified in order to work on Windows.

## Process to get up and running
1. Setting up Voice
2. Create a Router
3. Create a Skill
4. Create a Queue
5. Create a Plan
6. Create an Agent
7. Create Tasks

## Supporting documentation
* Predicate [expression guide](ExpressionSyntax.md) for Plans, Queues and Agents.
* Localhost OpenAPI spec http://localhost:8080/comms-router-web/swagger-ui.html
* Interactive Voice Response (IVR) guide https://developer.nexmo.com/voice/voice-api/guides/interactive-voice-response

### Prerequisites
* Setup a [Nexmo account](https://dashboard.nexmo.com/sign-up)
* Rent a virtual number using [Dashboard](https://dashboard.nexmo.com/buy-numbers) or [Developer API](https://developer.nexmo.com/api/developer/numbers) and set the webhook endpoint to your app

## 1. Setting up Voice
An overview and Getting started guide for Voice can be found here https://developer.nexmo.com/voice/voice-api/overview

* [Create an application](https://developer.nexmo.com/concepts/guides/applications#apps_quickstart) and associate it with your virtual number. Nexmo retrieves the initial NCCO from the answer_url webhook and sends the DTMF input to the eventUrl webhook defined in the initial NCCO
* Create your NCCO using the IVR use case with the required Task and Agent capabilities (link to demo app) link it to your application (answer_url)
* Create client application to manage Comms Router and Voice or install and use the [Comms Router demo application](../applications/demo/)

## 2. Create a Router
When creating a router you can either specify the router `ref` ID for example `MY_ROUTER` or leave blank and it will be automatically generated. All other resources are identified by the their `ref` ID. When creating a router the `ref` ID must be unique.
```
curl -X PUT http://localhost:8080/comms-router-web/api/routers/MY_ROUTER
```
## 3. Create a Skill
For more example of how to correctly format Skills read our Predicate [expression guide](ExpressionSyntax.md) for Skills, Agents and Queues.
```curl
curl -X PUT http://localhost:8080/comms-router-web/api/routers/MY_ROUTER/skills/MY_SKILL \
  -H 'Content-Type:application/json' \
  -d$'{"name":"NAME", "description":"DESCRIPTION", "multivalue":"true", "domain":{"type": "enumeration", "values": ["1","2","3"]}}'
```

## 4. Create a Queue
```curl
curl -X PUT http://localhost:8080/comms-router-web/api/routers/MY_ROUTER/queues/MY_QUEUE \
  -H 'Content-Type:application/json' \
  -d$'{"predicate":"HAS(#{language},\'en\')"}}'  
```
**Note:** When creating a Queue we recommend using `PUT` method as it is possible to add the Queue name in `path` as otherwise the `routerRef` parameter is required in the header to name the Queue.

## 5. Create a Plan
The following Plan called `MY_PLAN` has a description and rules which include tasks must have `language == en` and will be routed to `MY_QUEUE` with a priority and timeout set. If no Agents are available in this Queue or if tineout limit reached with no other Queues the Task will move to the `DEFAULT_QUEUE`.

In the example below make sure you have a Queue created otherwise it is possible to create a Plan without queue specifications. Let's create a Plan with a rule that will route Tasks requiring Spanish speaking Agent in our Spanish queue. Tasks that don't match this rule we will route to the English Queue.
```curl
curl -X PUT http://localhost:8080/comms-router-web/api/routers/MY_ROUTER/plans/MY_PLAN \
  -H 'Content-Type:application/json' \
  -d$'{"description":"Description of MY_PLAN", "rules":[{"tag":"english", "predicate":"#{language} == \'en\'", "routes":[{"queueRef":"MY_QUEUE", "priority":3, "timeout":300}]}], "defaultRoute":{"queueRef":"MY_QUEUE", "priority":0, "timeout":0}}}'
```

## 6. Create an Agent
Agent can be created using a defined `ref` or allow this to be automatically created by the service. The example below creates a new user called `ALEX` who has a SIP URI and English `en` language capability. When creating an Agent the `ref` ID must be unique.
```curl
curl -X PUT http://localhost:8080/comms-router-web/api/MY_ROUTER/demo/agents/ALEX \
  -H 'Content-Type:application/json' \
  -d '{"address":"sip:alex@vonage.com","capabilities":{"language":["en"]}}'
```
If not specified an Agent's status will be set to `offline` by default. Other statuses include `offline`, `ready`, `busy`, `unavailable`.

### List Agents
#### Request
```curl
curl -X GET http://localhost:8080/comms-router-web/api/routers/MY_ROUTER/agents
```

#### Response
```curl
[
  {
    "ref": "alex",
    "routerRef": "MY_ROUTER",
    "capabilities": {
      "language": "en"
    },
    "address": "sip:alex@vonage.com",
    "state": "offline",
    "queueIds": [
      "queue-en"
    ]
  },
  {
    ...
  }
]
```

## 7. Create Tasks
Below is a request to create a task `MY_TASK` that requires an Agent to be able to speak English. We assign to it the plan `MY_PLAN` using `planRef` parameter. The `callbackUrl` parameter specifies the user application entry point to be called by the router for activity related with this Task.
```curl
curl -X PUT http://localhost:8080/comms-router-web/api/routers/MY_ROUTER/tasks/MY_TASK \
  -H 'Content-Type:application/json' \
  -d$'{"requirements":{"language":"en"},"planRef":"MY_PLAN","callbackUrl":"http://webhook.site/#/fae3ff2f-a1f7-4648-9804-21666b3bb15d"}'
```

In addition to using a Plan to route Tasks, the router accepts direct Queue assignment by the user application.
```curl
curl -X PUT http://localhost:8080/comms-router-web/api/routers/MY_ROUTER/tasks/MY_TASK \
  -H 'Content-Type:application/json' \
  -d$'{"queueRef":"MY_QUEUE","callbackUrl":"http://webhook.site/#/fae3ff2f-a1f7-4648-9804-21666b3bb15d"}'
```

### List Tasks
#### Request
```curl
curl -X GET http://localhost:8080/comms-router-web/api/routers/MY_ROUTER/tasks
```

#### Response
```json
[
  {
    "ref": "MY_TASK",
    "routerRef": "MY_ROUTER",
    "requirements": {
      "language": "en"
    },
    "userContext": null,
    "state": "waiting",
    "planRef": null,
    "queueRef": "MY_QUEUE",
    "agentRef": null,
    "callbackUrl": "http://webhook.site/#/fae3ff2f-a1f7-4648-9804-21666b3bb15d"
  },
  {
    ...
  }
]
```

By default all Tasks are in state `waiting` if all Agents are in state `offline`. To change an Agents status use the following command:
```curl
curl -X POST http://localhost:8080/comms-router-web/api/routers/MY_ROUTER/agents/ALEX \
  -H 'Content-Type:application/json' \
  -d '{"state":"ready"}'
```
Task statuses are managed by the Comms Router and include: `waiting`, `canceled`, `assigned`, `completed`.

## Task flow
Once the router assigns a Task an Agent the Agent status changes to `busy`. A call to the provided `callbackUrl` can be observed in `http://webhook.site/#/fae3ff2f-a1f7-4648-9804-21666b3bb15d`. 

When the user application is done with processing a Task it must declare it as done:
```curl
curl -X POST http://localhost:8080/comms-router-web/api/routers/MY_ROUTER/tasks/MY_TASK 
  -H 'Content-Type:application/json' 
  -d '{"state":"completed"}'
```

The router then releases the Agent and they are available (`ready`) for other Tasks. In this example the agent `ALEX` can serve more than one Queue for example `EN_QUEUE` and `FR_QUEUE`, it will automatically get the other Task we created.

```curl
curl http://localhost:8080/comms-router-web/api/routers/MY_ROUTER/tasks
```

```json
[
  {
    "ref": "EN_TASK",
    "routerRef": "MY_ROUTER",
    "requirements": {
      "language": "en"
    },
    "userContext": null,
    "state": "completed",
    "planId": null,
    "queueRef": "EN_QUEUE",
    "agentRef": null,
    "callbackUrl": "http://webhook.site/#/fae3ff2f-a1f7-4648-9804-21666b3bb15d"
  },
  {
    "ref": "FR_TASK",
    "routerRef": "MY_ROUTER",
    "requirements": null,
    "userContext": null,
    "state": "assigned",
    "planRef": null,
    "queueRef": "FR_QUEUE",
    "agentRef": "ALEX",
    "callbackUrl": "http://webhook.site/#/fae3ff2f-a1f7-4648-9804-21666b3bb15d"
  }
]
```

To clean up the flow we should finish by making the Task complete.
```curl
curl -X POST http://localhost:8080/comms-router-web/api/routers/MY_ROUTER/tasks/EN_TASK \
  -H 'Content-Type:application/json'
  -d '{"state":"completed"}'
```
