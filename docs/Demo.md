Demo environment
===

Demo is using phone number 18885300772

### Inbound call
Call to 18885300772 will connect you to an agent.

### Agent

1. Agent's phone can be set with the following bash command:

``` shell
phone=12146664321 && curl -v -X POST 'https://comms-router:9gqSq4GN9\{&HtrQs@api.comms-router.com:443/comms-router-web/api'/routers/router-1/agents/r8AzfepLFqVfUGU7wgQOo6 -H 'Content-type:application/json' -d '{"address":"'$phone'"}'

```


2. Status of the agent can be seen with:

``` shell
curl -v -X GET 'https://comms-router:9gqSq4GN9\{&HtrQs@api.comms-router.com:443/comms-router-web/api'/routers/router-1/agents/r8AzfepLFqVfUGU7wgQOo6 -H 'Content-type:application/json'
```

### Queue

1. List waiting tasks:

``` shell
curl -s -X GET 'https://comms-router:9gqSq4GN9\{&HtrQs@api.comms-router.com:443/comms-router-web/api'/routers/router-1/queues/queue-demo/tasks -H 'Content-type:application/json'
```
