::All of the comms-router commands listen in the README.

@echo off
title comms-router windows commands

::for testing - https://requestb.in/1koh4zk1?inspect

IF NOT [%1] == [] (
	SET callback=%1
)

IF [%1] == [] (
	SET /p callback= "Enter Callback Url: " 
)

echo[

echo Create a router, providing it's ID:
curl -X PUT http://localhost:8080/comms-router-web/api/routers/my-router

echo[
echo[

echo Create some queues.
curl -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/queues/queue-en -H "Content-Type:application/json" -d "{\"predicate\":\"HAS(#{language},'en')\"}}"

echo[

curl -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/queues/queue-es -H "Content-Type:application/json" -d "{\"predicate\":\"HAS(#{language},'es')\"}}"

echo[
echo[

echo Create agents.
curl -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/agents/alice -H "Content-Type:application/json" -d "{\"address\":\"sip:alice@comms-router.org\",\"capabilities\":{\"language\":[\"en\"]}}"

echo[

curl -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/agents/juan -H "Content-Type:application/json" -d "{\"address\":\"sip:juan@comms-router.org\",\"capabilities\":{\"language\":[\"es\"]}}"

echo[

curl -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/agents/maria -H "Content-Type:application/json" -d "{\"address\":\"sip:maria@comms-router.org\",\"capabilities\":{\"language\":[\"en\",\"es\"]}}"

echo[
echo[

echo List agents.
curl http://localhost:8080/comms-router-web/api/routers/my-router/agents

echo[
echo[

echo Create a plan.
curl -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/plans/by-language -H "Content-Type:application/json" -d "{\"rules\":[{\"tag\":\"spanish\",\"predicate\":\"#{language} = 'es'\",\"queueId\":\"queue-es\"},{\"tag\":\"default-english\",\"predicate\":\"true\",\"queueId\":\"queue-en\"}]}"

echo[
echo[

echo Create tasks.
curl -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/tasks/task-es -H "Content-Type:application/json" -d "{\"requirements\":{\"language\":\"es\"},\"planId\":\"by-language\",\"callbackUrl\":\"%callback%\"}"

echo[
echo[

echo In addition to using a plan to route tasks, the user accepts direct queue assignment by the user application.
curl -X PUT http://localhost:8080/comms-router-web/api/routers/my-router/tasks/task-en -H "Content-Type:application/json" -d "{\"queueId\":\"queue-en\",\"callbackUrl\":\"%callback%\"}"

echo[
echo[

echo List tasks.
curl http://localhost:8080/comms-router-web/api/routers/my-router/tasks

echo[
echo[

echo Change agent's state.
curl -X POST http://localhost:8080/comms-router-web/api/routers/my-router/agents/maria -H "Content-Type:application/json" -d "{\"state\":\"ready\"}"

echo[

echo Complete Task.
curl -X POST http://localhost:8080/comms-router-web/api/routers/my-router/tasks/task-es -H "Content-Type:application/json" -d "{\"state\":\"completed\"}"

echo[

echo Marking the task completed.
curl -X POST http://localhost:8080/comms-router-web/api/routers/my-router/tasks/task-en -H "Content-Type:application/json" -d "{\"state\":\"completed\"}"

echo[

echo List tasks.
curl http://localhost:8080/comms-router-web/api/routers/my-router/tasks

pause>nul


