# Replace or create agent.
curl -s -X PUT 'http://localhost:8080/comms-router-web/api/routers/router-ivr/agents/en-es-support' -H 'Content-type:application/json' -d "{\"address\":\"12312377880\",\"name\":\"Pablo Jenkins\",\"capabilities\":{\"language\":[\"en\",\"es\"],\"department\":\"support\"}}"
# Response:{"ref":"en-es-support"}

# Replace or create agent.
curl -s -X PUT 'http://localhost:8080/comms-router-web/api/routers/router-ivr/agents/en-sales' -H 'Content-type:application/json' -d "{\"address\":\"12017621651\",\"name\":\"John Seller\",\"capabilities\":{\"language\":[\"en\"],\"department\":\"sales\"}}"
# Response:{"ref":"en-sales"}

# Replace or create agent.
curl -s -X PUT 'http://localhost:8080/comms-router-web/api/routers/router-ivr/agents/es-sales' -H 'Content-type:application/json' -d "{\"address\":\"12017621652\",\"name\":\"Domingo Secada\",\"capabilities\":{\"language\":[\"es\"],\"department\":\"sales\"}}"
# Response:{"ref":"es-sales"}

# Create new plan to queue NULL with predicate 1 ==1.
curl -s -X PUT 'http://localhost:8080/comms-router-web/api/routers/router-ivr/plans/simple-menu' -H 'Content-type:application/json' -d "{\"rules\":[{\"tag\":\"en-sales\",\"predicate\":\"#{language}=='en' && #{department}=='sales'\",\"routes\":[{\"queueRef\":\"en-sales\",\"priority\":0,\"timeout\":3600}]},{\"tag\":\"es-sales\",\"predicate\":\"#{language}=='es' && #{department}=='sales'\",\"routes\":[{\"queueRef\":\"es-sales\",\"priority\":0,\"timeout\":3600}]},{\"tag\":\"en-support\",\"predicate\":\"#{language}=='en' && #{department}=='support'\",\"routes\":[{\"queueRef\":\"en-support\",\"priority\":0,\"timeout\":3600}]},{\"tag\":\"es-support\",\"predicate\":\"#{language}=='es' && #{department}=='support'\",\"routes\":[{\"queueRef\":\"es-support\",\"priority\":0,\"timeout\":3600}]}],\"description\":\"description\",\"defaultRoute\":{\"queueRef\":\"queue-ivr\",\"priority\":0,\"timeout\":3600}}"
# Response:{"ref":"simple-menu"}
