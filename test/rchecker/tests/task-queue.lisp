(in-package #:rchecker)


(defun setup-rqpt (&key (qpredicate "1==1")
                     (tpredicate "1")
                     (task-req (jsown:new-js ("bool" t)
                                             ("digit" 10)
                                             ("array" (list 1 2 3 4))
                                             ;;("float" 10.5)
                                             ("string" "string")
                                             ))
                     (fn #'(lambda(router-id queue-id plan-id task-id)
                             (tstep-result "Test completed" "Success" t "Setup router queue plan task completed" (list "OK"))) ))

  (tlet ((router-id (js-val "id")
                    (tstep "Create new router"
                           (tapply (http-post "/routers" (jsown:new-js("name" "name")
                                                                      ("description" "description"))))
                           (check-and (has-json) (has-key "id"))))
         (queue-id (js-val "id")
                   (tstep "Create new queue"
                          (tapply (http-post (list "/routers" router-id "queues")
                                             (jsown:new-js ("description" "queue description")
                                                           ("predicate" qpredicate))))
                          (check-and (has-json) (has-key "id"))))
         (plan-id (js-val "id")
                  (eplan-new :router-id router-id
                             :queue-id queue-id
                             :default-queue-id queue-id
                             :predicate tpredicate :description "plan description"
                             :checks (check-and (has-json) (has-key "id") (publish-id :plan))))
         (task-id (js-val "id")
                  (tand
                   (tstep "Show created plan"
                          (tapply (http-get "/routers" router-id "plans" plan-id))
                          (check-and (has-json) (has-key "id")))
                   (tstep "Create new task with plan"
                          (tapply (http-post (list "/routers" router-id "tasks")
                                             (jsown:new-js ("callbackUrl" "http://localhost:8080")
                                                           ("requirements" task-req)
                                                           ("queueId" :null)
                                                           ("planId" plan-id))))
                          (check-and (has-json) (has-key "id"))))))
    (funcall fn router-id queue-id plan-id task-id)))

(defun setup-rqt (&key (qpredicate "1==1")
                    (fn #'(lambda(router-id queue-id plan-id agent-id)
                             (tstep-result "End of test" "Success" t  "Setup router queue agent completed" (list "OK"))) ))

  (tlet ((router-id (js-val "id")
                    (tstep "Create new router"
                           (tapply (http-post "/routers" (jsown:new-js("name" "name")
                                                                      ("description" "description"))))
                           (check-and (has-json) (has-key "id"))))
         (queue-id (js-val "id")
                   (tstep "Create new queue"
                          (tapply (http-post (list "/routers" router-id "queues")
                                             (jsown:new-js ("description" "queue description")
                                                           ("predicate" qpredicate))))
                          (check-and (has-json) (has-key "id"))))
         (agent-id (js-val "id")
                   (tstep "Create agent"
                          (tapply (http-post (list "/routers" router-id "agents") (jsown:new-js
                                                                                    ("address" "some-phone")
                                                                                    ("capabilities"
                                                                                     (jsown:new-js ("language" "en"))))))
                          (check-and (has-json) (has-key "id"))))
         (task-id (js-val "id")
                  (tstep "Create new task with plan"
                         (tapply (http-post (list "/routers" router-id "tasks")
                                            (jsown:new-js ("callbackUrl" "http://localhost:8080")
                                                          ("requirements" :null)
                                                          ("queueId" queue-id)
                                                          ("planId" :null))))
                         (check-and (has-json) (has-key "id")))))
    (funcall fn router-id queue-id nil task-id)))

(defun process-one-task()
  #'(lambda (router-id queue-id plan-id task-id)
      (tand
       (tstep "Check that task is in state waiting."
              (tapply (http-get "/routers" router-id "tasks" task-id ))
              (check-and (has-json) (has-kv "state" "waiting")
                         (has-kv "queueId" queue-id)))
       (tlet ((agent-id (js-val "id")
                        (tstep "Create agent"
                               (tapply (http-post (list "/routers" router-id "agents") (jsown:new-js
                                                                                         ("address" "some-phone")
                                                                                         ("capabilities"
                                                                                          (jsown:new-js ("language" "en"))))))
                               (check-and (has-json) (has-key "id")))))
         (tand
          (tstep "Check agent to be offline."
                 (tapply (http-get "/routers" router-id "agents" agent-id))
                 (check-and (has-json) (has-kv "state" "offline")))
          (tstep "Set agent to be ready."
                 (tapply (http-post (list "/routers" router-id "agents" agent-id)
                                    (jsown:new-js
                                      ("state" "ready"))))
                 (check-and (is-equal "")))
          (twait (tstep "Check that task is in state assigned."
                        (tapply (http-get "/routers" router-id "tasks" task-id ))
                        (check-and (has-json) (has-kv "state" "assigned")
                                   (has-kv "queueId" queue-id)
                                   (has-kv "agentId" agent-id)
                                   )))
          (tstep "Agent has task.Check that it gets busy."
                 (tapply (http-get "/routers" router-id "agents" agent-id))
                 (check-and (has-json) (has-kv "state" "busy")))
          (tstep "Complete the task."
                 (tapply (http-post (list "/routers" router-id "tasks" task-id)
                                    (jsown:new-js ("state" "completed"))))
                 (is-equal ""))
          (twait (tstep "Task is completed, so agent should be 'ready'."
                        (tapply (http-get "/routers" router-id "agents" agent-id))
                        (check-and (has-json) (has-kv "state" "ready")))))))))

(defun process-two-tasks()
  #'(lambda(router-id queue-id plan-id task-id)
      (tlet((high-task-id (js-val "id")
                          (tstep "Create high priority task"
                                 (tapply (http-post (list "/routers" router-id "tasks")
                                                    (jsown:new-js ("callbackUrl" "http://localhost:8080")
                                                                  ("requirements" (jsown:new-js ("key" t)))
                                                                  ("queueId" queue-id)
                                                                  ("planId" :null))))
                                 (check-and (has-json) (has-key "id")))))
        (tand
         (tstep "Check that high priority task is in state waiting."
                (tapply (http-get "/routers" router-id "tasks" high-task-id ))
                (check-and (has-json) (has-kv "state" "waiting")
                           (has-kv "queueId" queue-id)))
         (tstep "Check that low priority task is in state waiting."
                (tapply (http-get "/routers" router-id "tasks" task-id ))
                (check-and (has-json) (has-kv "state" "waiting")
                           (has-kv "queueId" queue-id)))

         (tlet ((agent-id (js-val "id")
                          (tstep "Create agent"
                                 (tapply (http-post (list "/routers" router-id "agents") (jsown:new-js
                                                                                           ("address" "some-phone")
                                                                                           ("capabilities"
                                                                                            (jsown:new-js ("language" "en"))))))
                                 (check-and (has-json) (has-key "id")))))
           (tand
            (tstep "Check agent to be offline."
                   (tapply (http-get "/routers" router-id "agents" agent-id))
                   (check-and (has-json) (has-kv "state" "offline")))
            (tstep "Set agent to be ready."
                   (tapply (http-post (list "/routers" router-id "agents" agent-id)
                                      (jsown:new-js
                                        ("state" "ready"))))
                   (check-and (is-equal "")))
            (tstep "Check that high priority task is in state assigned."
                   (tapply (http-get "/routers" router-id "tasks" high-task-id ))
                   (check-and (has-json) (has-kv "state" "assigned")
                              (has-kv "queueId" queue-id)
                              (has-kv "agentId" agent-id)
                              ))
            (tstep "Agent has task.Check that it gets busy."
                   (tapply (http-get "/routers" router-id "agents" agent-id))
                   (check-and (has-json) (has-kv "state" "busy")))
            (tstep "Complete the task."
                   (tapply (http-post (list "/routers" router-id "tasks" high-task-id)
                                      (jsown:new-js ("state" "completed"))))
                   (is-equal ""))
            (twait (tstep "Task is completed, but there are more tasks, so agent should start the next one."
                    (tapply (http-get "/routers" router-id "agents" agent-id))
                    (check-and (has-json) (has-kv "state" "busy"))))

            (tstep "Check that low priority task is in state assigned."
                   (tapply (http-get "/routers" router-id "tasks" task-id ))
                   (check-and (has-json) (has-kv "state" "assigned")
                              (has-kv "queueId" queue-id)
                              (has-kv "agentId" agent-id)))
            (tstep "Complete the task."
                   (tapply (http-post (list "/routers" router-id "tasks" task-id)
                                      (jsown:new-js ("state" "completed"))))
                   (is-equal ""))
            (twait (tstep "Task is completed, so agent should be 'ready'."
                          (tapply (http-get "/routers" router-id "agents" agent-id))
                          (check-and (has-json) (has-kv "state" "ready")))) ) ) ) ) ) )

(defun test-complete-task(&key (qpredicate "1==1") (tpredicate "1") (task-req (jsown:new-js ("bool" t)
                                                                                           ("digit" 10)
                                                                                           ("array" (list 1 2 3 4))
                                                                                           ;;("float" 10.5)
                                                                                           ("string" "string"))))
  (tand
   (setup-rqpt :qpredicate qpredicate :tpredicate tpredicate :task-req task-req
               :fn (process-one-task))
   (setup-rqt :qpredicate qpredicate
              :fn (process-one-task)) ) )

(defun test-delete-agent()
  (setup-rqpt
   :qpredicate "1==1" :tpredicate "1"
   :fn #'(lambda(router-id queue-id plan-id task-id)
           (tand
            (funcall (process-one-task) router-id queue-id plan-id task-id)
            (tlet((agent-id (js-val "id") #'first
                            (eagent-all :router-id router-id)))
              (tand
               (eagent-del :router-id router-id :id agent-id)
               (etask-all :router-id router-id)) ) ) )) )

(defun test-set-unavailable()
  (tlet ((router-id (js-val "id") (erouter-new))
         (queue-id (js-val "id") (equeue-new :router-id router-id))
         (task-id (js-val "id") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://google.com/not-existing-page"))
         (agent-id (js-val "id") (eagent-new :router-id router-id)))
    (tand
     (eagent-set  :router-id router-id :id agent-id :state "ready")
     (twait (etask :router-id router-id :id task-id :checks (check-and (has-kv "state" "assigned")
                                                                       (has-kv "agentId" agent-id))))
     (etask-set   :router-id router-id :id task-id  :state "waiting")
     (twait (eagent :router-id router-id :id agent-id :checks (has-kv "state" "unavailable")))
     (eagent-set :router-id router-id :id agent-id :description "Restore agent to working state" :state "ready")
     (twait (eagent :description "Canceled task gets to the same agent":router-id router-id :id agent-id :checks (has-kv "state" "busy"))))))

(defun test-task-ordering()
  (tlet ((router-id (js-val "id") (erouter-new))
         (queue-id (js-val "id") (equeue-new :router-id router-id))
         (task-id (js-val "id") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/"))
         (task1-id (js-val "id") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/"))
         (agent-id (js-val "id") (eagent-new :router-id router-id)))
    (tand
     (eagent-set :router-id router-id :state "ready")
     (twait (eagent :router-id router-id :id agent-id :checks (has-kv "state" "busy")))
     (etask :description "Check that first task is the selected one"
            :router-id router-id
            :id task-id
            :checks (has-kv "agentId" agent-id))
     (tlet ((task2-id (js-val "id") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://google.com/")))
       (etask-set :state "completed"
                  :id task-id
                  :router-id router-id)
       (twait (eagent :router-id router-id :id agent-id :checks (has-kv "state" "busy")))
       (etask :description "Check that second task is the selected one"
              :router-id router-id
              :id task1-id
              :checks (check-and (has-kv "agentId" agent-id)
                                 (has-kv "state" "assigned")))
       (etask-set :state "completed"
                  :id task1-id
                  :router-id router-id)
       (twait (eagent :router-id router-id :id agent-id :checks (has-kv "state" "busy")))
       (etask :description "Check that second task is the selected one"
              :router-id router-id
              :id task2-id
              :checks (check-and (has-kv "agentId" agent-id)
                                 (has-kv "state" "assigned")))
       (etask-set :state "completed"
                  :id task2-id
                  :router-id router-id) ))))

(defun test-task-ordering1()
  (tlet ((router-id (js-val "id") (erouter-new))
         (queue-id (js-val "id") (equeue-new :router-id router-id))
         (agent-id (js-val "id") (eagent-new :router-id router-id))
         (agent-ready (eagent-set :id agent-id :state "ready"
                                  :router-id router-id))
         (task-id (js-val "id") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/"))
         (agent1-id (js-val "id") (eagent-new :router-id router-id))
         (agent1-ready (eagent-set :id agent1-id :state "ready"
                                   :router-id router-id))
         (task1-id (js-val "id") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/")))
    (tand
     (etask-set :id task1-id :router-id router-id :state "completed")
     (twait (eagent :router-id router-id :id agent1-id :checks (has-kv "state" "ready")))
     (etask-set :id task-id :router-id router-id :state "completed")
     (twait (eagent :router-id router-id :id agent-id :checks (has-kv "state" "ready")))
     (tlet ((task2-id (js-val "id") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/")))
       (tand
        (twait (etask :description "Check that task handled not by agent that just has completed task, but the other one."
                      :router-id router-id
                      :id task2-id
                      :checks (has-kv "state" "assigned")))
        (etask :description "Check that task handled not by agent that just has completed task, but the other one."
               :router-id router-id
               :id task2-id
               :checks (has-kv "agentId" agent1-id)))))))

(defun test-task-ordering2()
  (tlet ((router-id (js-val "id") (erouter-new))
         (queue-id (js-val "id") (equeue-new :router-id router-id))
         (agent-id (js-val "id") (eagent-new :router-id router-id))
         (agent-ready (eagent-set :id agent-id :state "ready"
                                  :router-id router-id))
         (agent1-id (js-val "id") (eagent-new :router-id router-id))
         (agent1-ready (eagent-set :id agent1-id :state "ready"
                                   :router-id router-id))
         (task-id (js-val "id") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/")))
    (tand
     (twait (etask :description "Wait state to be assigned."
                   :router-id router-id
                   :id task-id
                   :checks (has-kv "state" "assigned")))
     (etask :description "Check that task is handled first created agent."
            :router-id router-id
            :id task-id
            :checks (has-kv "agentId" agent-id)))))

(defun test-set-context(&key(router-id (get-event :router)) (queue-id (get-event :queue)))
  (tlet ((task-id (js-val "id")
                  (tstep "Create task"
                         (tapply (http-post (list "/routers" router-id "tasks")
                                            (jsown:new-js ("callbackUrl" (format nil "http://localhost:4343/nowheretaskk?router=~A&sleep=~A" router-id (random 2)))
                                                          ("requirements" (jsown:new-js ("key" t)))
                                                          ("queueId" queue-id)
                                        ;("userContext" (jsown:new-js ))
                                                          ("planId" :null))))
                         (check-and (has-json) (has-key "id")))))
    (etask-set-context :router-id router-id :task-id task-id :key "key" :value "value")))

(defun push-a-task(&key (host "localhost") (timeout 30) (handle-time 2) (shuffle-time #'identity))
  #'(lambda(&key (router-id (get-event :router)) (queue-id (get-event :queue)))
      (tlet ((task-id (js-val "id")
                  (tstep "Create task"
                         (tapply (http-post (list "/routers" router-id "tasks")
                                            (jsown:new-js ("callbackUrl"(format nil "http://~A:4343/task?router=~A&sleep=~A" host router-id
                                                                                (funcall shuffle-time 2)))
                                                          ("requirements" (jsown:new-js ("key" t)))
                                                          ("queueId" queue-id)
                                                          ("userContext" (jsown:new-js ))
                                                          ("planId" :null))))
                         (check-and (has-json) (has-key "id")))))
    (tand
     (twait (tstep "Wait task to be completed" (tapply (http-get "/routers" router-id "tasks" task-id ))
                   (check-and (has-json) (has-kv "state" "completed")))
            :delay 3 :timeout timeout)
     (tstep "Ensure that there where no errors on handling task by checking userContext.result."
            (tapply (http-get "/routers" router-id "tasks" task-id "user_context" "result"))
            (is-equal "true"))))))

(defun test-push-tasks (&key (tasks 10)(queues 1) (agents 1)
                          (push (push-a-task :host "localhost")))
  #'(lambda()
      (router-new)

      (let* ((q-ids (mapcar (js-val "id")
                            (lparallel:pmapcar #'(lambda(i) (queue-new))
                                               (loop :repeat queues :collect 1)) ))
             (agent-ids (lparallel:pmapcar #'(lambda(i) (agent-new ))
                                           (loop :repeat agents :collect 1)))
             (agent-ready (lparallel:pmapcar #'(lambda(json)(agent-set :id (jsown:val json "id") :state "ready"))
                                             agent-ids))
             (result (remove-if
                      #'second
                      (lparallel:pmapcar
                       #'(lambda(n)(funcall (funcall push  :queue-id (alexandria:random-elt q-ids))))
                       (loop :repeat tasks :collect 1) ))))
        (mapcar #'print-log result)) ) )

(defun test-plan-and-task()
  (tlet ((router-id (js-val "id")
                    (erouter-new))
         (default-queue-id (js-val "id")
                   (equeue-new :router-id router-id))
         (plan-id (js-val "id")
                  (eplan-new
                   :rules (list (jsown:new-js ("tag" :null)
                                              ("predicate" "HAS([10],#{age})")
                                              ("routes" (list  ))))
                   :default-route (jsown:new-js
                                    ("queueId" default-queue-id))
                   :router-id router-id)))
    (etask-new :queue-id :null :plan-id plan-id)))

(defun test-all(&key (tests (list (test-task-ordering)
                                  (test-task-ordering1)
                                  (test-task-ordering2)
                                  (test-set-unavailable)
                                  (test-delete-agent)
                                  (test-set-context)
                                  (test-plan-and-task)
                                  (test-complete-task))))
  (mapcar
   #'print-log
   (remove-if
    #'second
    (mapcar
     #'funcall tests))))

(defun delete-completed-tasks()
  (loop for task-all = (task-all) for task = (when (listp task-all)(first task-all)) :while (and task (equal (jsown:val task "state") "completed")) do (task-del :id (jsown:val task "id"))))

(defun setup-demo()
  (router-put :id "router-1")
  (queue-put :id "demo-queue")
  (agent-put :id "r8AzfepLFqVfUGU7wgQOo6"))

(defun create-tasks(&key (router-id (get-event :router)) (queue-id (get-event :queue)) (count 10))
  (time (remove-if #'second (lparallel:pmapcar #'(lambda(i)(funcall (etask-new :router-id router-id :queue-id queue-id))) (loop :repeat count :collect 1)))))

(defun test-performance (&key (tasks 10)(queues 1) (agents 1) (handle-time 0))
  #'(lambda()
      (router-new)

      (let* ((q-ids (mapcar (js-val "id")
                            (lparallel:pmapcar #'(lambda(i) (queue-new))
                                               (loop :repeat queues :collect 1)) ))
             (agent-ids (lparallel:pmapcar #'(lambda(i) (agent-new ))
                                           (loop :repeat agents :collect 1)))
             (result (time(remove-if
                           #'second
                           (lparallel:pmapcar
                            #'(lambda(n)(funcall (etask-new
                                                  :queue-id (alexandria:random-elt q-ids)
                                                  :callback-url (format nil "http://localhost:4343/task?router=fbjFl8BWnmnuM8TXtWolI6&sleep=~A"
                                                                        handle-time))))
                            (loop :repeat tasks :collect 1) ))))
             (agent-ready (time (lparallel:pmapcar #'(lambda(json)(agent-set :id (jsown:val json "id") :state "ready"))
                                                   agent-ids))) )
        (time (loop for queue = (lparallel:pevery #'(lambda(id) (zerop (jsown:val (queue-size :id id) "size")))
                                                  (mapcar (js-val "id") (queue-all)))
                 :while (not queue)  do (sleep 1)))
        (mapcar #'print-log result)) ) )
