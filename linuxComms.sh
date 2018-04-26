set -e 
#for testing - https://requestb.in/1koh4zk1?inspect

if [ "$1" = "" ]
then
    read -p "Enter Callback Url: " callback
else
    callback = $1;
fi

echo

echo Create a router, providing it’s Ref ID:
curl -s -X PUT http://localhost:8080/comms-router-web/api/routers/my-router

echo
echo

echo Create some queues.
curl -s -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/queues/queue-en -H 'Content-Type:application/json' -d '{"predicate":"language=in=(en)"}'

echo

curl -s -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/queues/queue-es -H 'Content-Type:application/json' -d '{"predicate":"language=in=(es)"}'

echo
echo

echo Create agents.
curl -s -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/agents/alice -H 'Content-Type:application/json' -d'{"address":"sip:alice@comms-router.org","capabilities":{"language":["en"]}}'

echo

curl -s -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/agents/juan -H 'Content-Type:application/json' -d '{"address":"sip:juan@comms-router.org","capabilities":{"language":["es"]}}'

echo

curl -s -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/agents/maria -H 'Content-Type:application/json' -d'{"address":"sip:maria@comms-router.org","capabilities":{"language":["en","es"]}}'

echo
echo

echo List agents and note the queue assignments:
curl -s http://localhost:8080/comms-router-web/api/routers/my-router/agents

echo
echo

echo Create a plan.
curl -s -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/plans/by-language -H 'Content-Type:application/json' -d '{"description":"put your plan description", "rules":[{"tag":"spanish", "predicate":"language==es", "routes":[{"queueRef":"queue-es", "priority":3, "timeout":300}, {"priority":10, "timeout":800}]}], "defaultRoute":{"queueRef":"queue-en"}}'

echo
echo

echo Create tasks.
curl -s -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/tasks/task-es -H 'Content-Type:application/json' -d '{"requirements":{"language":"es"},"planRef":"by-language","callbackUrl":'"\"$callback\""'}'

echo
echo

echo In addition to using a plan to route tasks, the user accepts direct queue assignment by the user application:
curl -s -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/tasks/task-en -H 'Content-Type:application/json' -d '{"queueRef":"queue-en","callbackUrl":'"\"$callback\""'}'

echo
echo

echo Let’s list the tasks and see the queues assigned:
curl -s http://localhost:8080/comms-router-web/api/routers/my-router/tasks

echo
echo

echo Change agent’s state.
ETAG=`curl -sI http://localhost:8080/comms-router-web/api/routers/my-router/agents/maria | grep ETag | cut -f2 -d\ `
curl -X POST http://localhost:8080/comms-router-web/api/routers/my-router/agents/maria -H 'Content-Type:application/json' -H "IF-MATCH:$ETAG" -d '{"state":"ready"}'
sleep 3
echo

echo Complete Task.
curl -s -X POST http://localhost:8080/comms-router-web/api/routers/my-router/tasks/task-es -H 'Content-Type:application/json' -d '{"state":"completed"}'

echo

echo We should finish our journey by making this task completed:
curl -s -X POST http://localhost:8080/comms-router-web/api/routers/my-router/tasks/task-en -H 'Content-Type:application/json' -d '{"state":"completed"}'

echo

read -p "Press enter to continue"
