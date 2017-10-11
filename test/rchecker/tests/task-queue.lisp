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
                             (tstep-result "Success" t "Setup router queue plan task completed" (list "OK"))) ))

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
                  (tstep "Create new plan"
                         (tapply (http-post (list "/routers" router-id "plans")
                                            (jsown:new-js
                                              ("rules" (list
                                                        (jsown:new-js ("tag" "test-rule")
                                                                      ("predicate" tpredicate)
                                                                      ("queueId" queue-id))))
                                              ("description" "plan description"))))
                         (check-and (has-json) (has-key "id"))))
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
                    (fn #'(lambda(router-id queue-id agent-id)
                             (tstep-result "Success" t "Setup router queue agent completed" (list "OK"))) ))

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
                                                          ("requirements" task-req)
                                                          ("queueId" queue-id)
                                                          ("planId" :null))))
                         (check-and (has-json) (has-key "id")))))
    (funcall fn router-id queue-id plan-id task-id)))

(defun complete-task-new(&key (qpredicate "1==1") (tpredicate "1") (task-req (jsown:new-js ("bool" t)
                                                                                           ("digit" 10)
                                                                                           ("array" (list 1 2 3 4))
                                                                                           ;;("float" 10.5)
                                                                                           ("string" "string"))))
  (setup-rqpt :qpredicate qpredicate :tpredicate tpredicate :task-req task-req
              :fn #'(lambda (router-id queue-id plan-id task-id)
                      (tand
                       (tstep "Check that task is in state waiting."
                              (tapply (http-get "/routers" router-id "tasks" task-id ))
                              (check-and (has-json) (has-kv "state" "waiting")
                                         (has-kv "queueId" queue-id)
                                         (has-kv "agentId" nil)))
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
                          (tstep "Check that task is in state assigned."
                                 (tapply (http-get "/routers" router-id "tasks" task-id ))
                                 (check-and (has-json) (has-kv "state" "assigned")
                                            (has-kv "queueId" queue-id)
                                            (has-kv "agentId" agent-id)
                                            (has-kv "planId" plan-id)))
                          (tstep "Agent has task.Check that it gets busy."
                                 (tapply (http-get "/routers" router-id "agents" agent-id))
                                 (check-and (has-json) (has-kv "state" "busy")))
                          (tstep "Complete the task."
                                 (tapply (http-post (list "/routers" router-id "tasks" task-id)
                                                    (jsown:new-js ("state" "completed"))))
                                 (is-equal ""))
                          (tstep "Task is completed, so agent should be 'ready'."
                                 (tapply (http-get "/routers" router-id "agents" agent-id))
                                 (check-and (has-json) (has-kv "state" "ready")))))))))

(defun complete-task-with-priority(&key (qpredicate "1==1")
                                     (tpredicate "1")
                                     (task-req (jsown:new-js ("bool" t)
                                                             ("digit" 10)
                                                             ("array" (list 1 2 3 4))
                                                             ;;("float" 10.5)
                                                             ("string" "string"))))
  (setup-rqpt :qpredicate qpredicate :tpredicate tpredicate :task-req task-req
              :fn #'(lambda (router-id queue-id plan-id task-id)
                      (tlet((high-task-id (js-val "id")
                                          (tstep "Create high priority task"
                                                 (tapply (http-post (list "/routers" router-id "tasks")
                                                                    (jsown:new-js ("callbackUrl" "http://localhost:8080")
                                                                                  ("requirements" (jsown:new-js ("key" t)))
                                                                                  ("priority" 100)
                                                                                  ("queueId" queue-id)
                                                                                  ("planId" :null))))
                                                 (check-and (has-json) (has-key "id")))))
                        (tand
                         (tstep "Check that high priority task is in state waiting."
                                (tapply (http-get "/routers" router-id "tasks" high-task-id ))
                                (check-and (has-json) (has-kv "state" "waiting")
                                           (has-kv "queueId" queue-id)
                                           (has-kv "agentId" nil)))
                         (tstep "Check that low priority task is in state waiting."
                                (tapply (http-get "/routers" router-id "tasks" task-id ))
                                (check-and (has-json) (has-kv "state" "waiting")
                                           (has-kv "queueId" queue-id)
                                           (has-kv "agentId" nil)))
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
                                              (has-kv "planId" ())))
                            (tstep "Agent has task.Check that it gets busy."
                                   (tapply (http-get "/routers" router-id "agents" agent-id))
                                   (check-and (has-json) (has-kv "state" "busy")))
                            (tstep "Complete the task."
                                   (tapply (http-post (list "/routers" router-id "tasks" high-task-id)
                                                      (jsown:new-js ("state" "completed"))))
                                   (is-equal ""))
                            (tstep "Task is completed, but there are more tasks, so agent should start the next one."
                                   (tapply (http-get "/routers" router-id "agents" agent-id))
                                   (check-and (has-json) (has-kv "state" "busy")))

                            (tstep "Check that low priority task is in state waiting."
                                   (tapply (http-get "/routers" router-id "tasks" task-id ))
                                   (check-and (has-json) (has-kv "state" "assigned")
                                              (has-kv "queueId" queue-id)
                                              (has-kv "agentId" nil)))
                            ) ) )
                        )
                      )))

(defun complete-task-with-priority(&key (qpredicate "1==1") (tpredicate "1"))
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


         (low-task-id (js-val "id")
                      (tstep "Create low priority task"
                             (tapply (http-post (list "/routers" router-id "tasks")
                                                (jsown:new-js ("callbackUrl" "http://localhost:8080")
                                                              ("requirements" (jsown:new-js ("key" t)))
                                                              ("priority" 0)
                                                              ("queueId" queue-id)
                                                              ("planId" :null))))
                             (check-and (has-json) (has-key "id"))))
         (high-task-id (js-val "id")
                       (tstep "Create high priority task"
                              (tapply (http-post (list "/routers" router-id "tasks")
                                                 (jsown:new-js ("callbackUrl" "http://localhost:8080")
                                                               ("requirements" (jsown:new-js ("key" t)))
                                                               ("priority" 100)
                                                               ("queueId" queue-id)
                                                               ("planId" :null))))
                              (check-and (has-json) (has-key "id")))))
    (tand
     (tstep "Check that high priority task is in state waiting."
            (tapply (http-get "/routers" router-id "tasks" high-task-id ))
            (check-and (has-json) (has-kv "state" "waiting")
                       (has-kv "queueId" queue-id)
                       (has-kv "agentId" nil)))
     (tstep "Check that low priority task is in state waiting."
            (tapply (http-get "/routers" router-id "tasks" low-task-id ))
            (check-and (has-json) (has-kv "state" "waiting")
                       (has-kv "queueId" queue-id)
                       (has-kv "agentId" nil)))
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
                          (has-kv "planId" ())))
        (tstep "Agent has task.Check that it gets busy."
               (tapply (http-get "/routers" router-id "agents" agent-id))
               (check-and (has-json) (has-kv "state" "busy")))
        (tstep "Complete the task."
               (tapply (http-post (list "/routers" router-id "tasks" high-task-id)
                                  (jsown:new-js ("state" "completed"))))
               (is-equal ""))
        (tstep "Task is completed, but there are more tasks, so agent should start the next one."
               (tapply (http-get "/routers" router-id "agents" agent-id))
               (check-and (has-json) (has-kv "state" "busy")))

        (tstep "Check that low priority task is in state waiting."
               (tapply (http-get "/routers" router-id "tasks" low-task-id ))
               (check-and (has-json) (has-kv "state" "assigned")
                          (has-kv "queueId" queue-id)
                          (has-kv "agentId" nil)))
        ) ) )) )
