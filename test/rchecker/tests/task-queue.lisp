(in-package #:rchecker)
(defun delete-tasks(router-id)
  (delete-items 
   :all #'(lambda() (task-all :router-id router-id))
   :complete #'(lambda(task-id)
                 (let ((task-state (jsown:val (task :router-id router-id :id task-id) "state"))) 
                   (cond ((equal  task-state "assigned")
                          (funcall (etask-set :router-id router-id :id task-id :state "completed")) )
                         ((equal task-state "waiting")
                          (funcall (etask-set :router-id router-id  :id task-id :state "canceled")) )) ))
   :delete #'(lambda(task-id) (funcall (etask-del :router-id router-id :id task-id)) )))
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

  (tlet ((router-id (js-val "ref")
                    (tstep "Create new router"
                           (tapply (http-post "/routers" (jsown:new-js("name" "name")
                                                                      ("description" "description"))))
                           (check-and (has-json) (has-key "ref"))))
         (queue-id (js-val "ref")
                   (tstep "Create new queue"
                          (tapply (http-post (list "/routers" router-id "queues")
                                             (jsown:new-js ("description" "queue description")
                                                           ("predicate" qpredicate))))
                          (check-and (has-json) (has-key "ref"))))
         (plan-id (js-val "ref")
                  (eplan-new :router-id router-id
                             :queue-id queue-id
                             :default-queue-id queue-id
                             :predicate tpredicate :description "plan description"
                             :checks (check-and (has-json) (has-key "ref") (publish-id :plan))))
         (task-id (js-val "ref")
                  (tand
                   (tstep "Show created plan"
                          (tapply (http-get "/routers" router-id "plans" plan-id))
                          (check-and (has-json) (has-key "ref")))
                   (tstep "Create new task with plan"
                          (tapply (http-post (list "/routers" router-id "tasks")
                                             (jsown:new-js ("callbackUrl" "http://localhost:8080")
                                                           ("requirements" task-req)
                                                           ("queueRef" :null)
                                                           ("planRef" plan-id))))
                          (check-and (has-json) (has-key "ref"))))))
    (funcall fn router-id queue-id plan-id task-id)))

(defun setup-rqt (&key (qpredicate "1==1")
                    (fn #'(lambda(router-id queue-id plan-id agent-id)
                             (tstep-result "End of test" "Success" t  "Setup router queue agent completed" (list "OK"))) ))

  (tlet ((router-id (js-val "ref")
                    (tstep "Create new router"
                           (tapply (http-post "/routers" (jsown:new-js("name" "name")
                                                                      ("description" "description"))))
                           (check-and (has-json) (has-key "ref"))))
         (queue-id (js-val "ref")
                   (tstep "Create new queue"
                          (tapply (http-post (list "/routers" router-id "queues")
                                             (jsown:new-js ("description" "queue description")
                                                           ("predicate" qpredicate))))
                          (check-and (has-json) (has-key "ref"))))
         (agent-id (js-val "ref")
                   (tstep "Create agent"
                          (tapply (http-post (list "/routers" router-id "agents") (jsown:new-js
                                                                                    ("address" "some-phone")
                                                                                    ("capabilities"
                                                                                     (jsown:new-js ("language" "en"))))))
                          (check-and (has-json) (has-key "ref"))))
         (task-id (js-val "ref")
                  (tstep "Create new task with plan"
                         (tapply (http-post (list "/routers" router-id "tasks")
                                            (jsown:new-js ("callbackUrl" "http://localhost:8080")
                                                          ("requirements" :null)
                                                          ("queueRef" queue-id)
                                                          ("planRef" :null))))
                         (check-and (has-json) (has-key "ref")))))
    (funcall fn router-id queue-id nil task-id)))

(defun process-one-task()
  #'(lambda (router-id queue-id plan-id task-id)
      (tand
       (tstep "Check that task is in state waiting."
              (tapply (http-get "/routers" router-id "tasks" task-id ))
              (check-and (has-json) (has-kv "state" "waiting")
                         (has-kv "queueRef" queue-id)))
       (tlet ((agent-id (js-val "ref")
                        (tstep "Create agent"
                               (tapply (http-post (list "/routers" router-id "agents") (jsown:new-js
                                                                                         ("address" "some-phone")
                                                                                         ("capabilities"
                                                                                          (jsown:new-js ("language" "en"))))))
                               (check-and (has-json) (has-key "ref")))))
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
                                   (has-kv "queueRef" queue-id)
                                   (has-kv "agentRef" agent-id)
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
      (tlet((high-task-id (js-val "ref")
                          (tstep "Create high priority task"
                                 (tapply (http-post (list "/routers" router-id "tasks")
                                                    (jsown:new-js ("callbackUrl" "http://localhost:8080")
                                                                  ("requirements" (jsown:new-js ("key" t)))
                                                                  ("queueRef" queue-id)
                                                                  ("planRef" :null))))
                                 (check-and (has-json) (has-key "ref")))))
        (tand
         (tstep "Check that high priority task is in state waiting."
                (tapply (http-get "/routers" router-id "tasks" high-task-id ))
                (check-and (has-json) (has-kv "state" "waiting")
                           (has-kv "queueRef" queue-id)))
         (tstep "Check that low priority task is in state waiting."
                (tapply (http-get "/routers" router-id "tasks" task-id ))
                (check-and (has-json) (has-kv "state" "waiting")
                           (has-kv "queueRef" queue-id)))

         (tlet ((agent-id (js-val "ref")
                          (tstep "Create agent"
                                 (tapply (http-post (list "/routers" router-id "agents") (jsown:new-js
                                                                                           ("address" "some-phone")
                                                                                           ("capabilities"
                                                                                            (jsown:new-js ("language" "en"))))))
                                 (check-and (has-json) (has-key "ref")))))
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
                              (has-kv "queueRef" queue-id)
                              (has-kv "agentRef" agent-id)
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
                              (has-kv "queueRef" queue-id)
                              (has-kv "agentRef" agent-id)))
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
            (tlet((agent-id (js-val "ref") #'first
                            (eagent-all :router-id router-id)))
              (tand
               (eagent-del :router-id router-id :id agent-id)
               (etask-all :router-id router-id)) ) ) )) )

(defun test-set-unavailable()
  (tlet ((router-id (js-val "ref") (erouter-new))
         (queue-id (js-val "ref") (equeue-new :router-id router-id))
         (task-id (js-val "ref") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://google.com/not-existing-page"))
         (agent-id (js-val "ref") (eagent-new :router-id router-id)))
    (tand
     (eagent-set  :router-id router-id :id agent-id :state "ready")
     (twait (etask :router-id router-id :id task-id :checks (check-and (has-kv "state" "assigned")
                                                                       (has-kv "agentRef" agent-id))))
     (etask-set   :router-id router-id :id task-id  :state "waiting")
     (twait (eagent :router-id router-id :id agent-id :checks (has-kv "state" "unavailable")))
     (eagent-set :router-id router-id :id agent-id :description "Restore agent to working state" :state "ready")
     (twait (eagent :description "Canceled task gets to the same agent":router-id router-id :id agent-id :checks (has-kv "state" "busy"))))))

(defun test-task-ordering()
  (tlet ((router-id (js-val "ref") (erouter-new))
         (queue-id (js-val "ref") (equeue-new :router-id router-id))
         (task-id (js-val "ref") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/"))
         (task1-id (js-val "ref") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/"))
         (agent-id (js-val "ref") (eagent-new :router-id router-id)))
    (tand
     (eagent-set :router-id router-id :state "ready")
     (twait (eagent :router-id router-id :id agent-id :checks (has-kv "state" "busy")))
     (etask :description "Check that first task is the selected one"
            :router-id router-id
            :id task-id
            :checks (has-kv "agentRef" agent-id))
     (tlet ((task2-id (js-val "ref") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://google.com/")))
       (etask-set :state "completed"
                  :id task-id
                  :router-id router-id)
       (twait (eagent :router-id router-id :id agent-id :checks (has-kv "state" "busy")))
       (etask :description "Check that second task is the selected one"
              :router-id router-id
              :id task1-id
              :checks (check-and (has-kv "agentRef" agent-id)
                                 (has-kv "state" "assigned")))
       (etask-set :state "completed"
                  :id task1-id
                  :router-id router-id)
       (twait (eagent :router-id router-id :id agent-id :checks (has-kv "state" "busy")))
       (etask :description "Check that second task is the selected one"
              :router-id router-id
              :id task2-id
              :checks (check-and (has-kv "agentRef" agent-id)
                                 (has-kv "state" "assigned")))
       (etask-set :state "completed"
                  :id task2-id
                  :router-id router-id) ))))

(defun test-task-ordering1()
  (tlet ((router-id (js-val "ref") (erouter-new))
         (queue-id (js-val "ref") (equeue-new :router-id router-id))
         (agent-id (js-val "ref") (eagent-new :router-id router-id))
         (agent-ready (eagent-set :id agent-id :state "ready"
                                  :router-id router-id))
         (task-id (js-val "ref") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/"))
         (agent1-id (js-val "ref") (eagent-new :router-id router-id))
         (agent1-ready (eagent-set :id agent1-id :state "ready"
                                   :router-id router-id))
         (task1-id (js-val "ref") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/")))
    (tand
     (etask-set :id task1-id :router-id router-id :state "completed")
     (twait (eagent :router-id router-id :id agent1-id :checks (has-kv "state" "ready")))
     (etask-set :id task-id :router-id router-id :state "completed")
     (twait (eagent :router-id router-id :id agent-id :checks (has-kv "state" "ready")))
     (tlet ((task2-id (js-val "ref") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/")))
       (tand
        (twait (etask :description "Check that task handled not by agent that just has completed task, but the other one."
                      :router-id router-id
                      :id task2-id
                      :checks (has-kv "state" "assigned")))
        (etask :description "Check that task handled not by agent that just has completed task, but the other one."
               :router-id router-id
               :id task2-id
               :checks (has-kv "agentRef" agent1-id)))))))

(defun test-task-ordering2()
  (tlet ((router-id (js-val "ref") (erouter-new))
         (queue-id (js-val "ref") (equeue-new :router-id router-id))
         (agent-id (js-val "ref") (eagent-new :router-id router-id))
         (agent-ready (eagent-set :id agent-id :state "ready"
                                  :router-id router-id))
         (agent1-id (js-val "ref") (eagent-new :router-id router-id))
         (agent1-ready (eagent-set :id agent1-id :state "ready"
                                   :router-id router-id))
         (task-id (js-val "ref") (etask-new :router-id router-id :queue-id queue-id :callback-url "http://localhost:8080/")))
    (tand
     (twait (etask :description "Wait state to be assigned."
                   :router-id router-id
                   :id task-id
                   :checks (has-kv "state" "assigned")))
     (etask :description "Check that task is handled first created agent."
            :router-id router-id
            :id task-id
            :checks (has-kv "agentRef" agent-id)))))

(defun test-set-context(&key(router-id (get-event :router)) (queue-id (get-event :queue)))
  (tlet ((task-id (js-val "ref")
                  (tstep "Create task"
                         (tapply (http-post (list "/routers" router-id "tasks")
                                            (jsown:new-js ("callbackUrl" (format nil "http://localhost:4343/nowheretaskk?router=~A&sleep=~A" router-id (random 2)))
                                                          ("requirements" (jsown:new-js ("key" t)))
                                                          ("queueRef" queue-id)
                                        ;("userContext" (jsown:new-js ))
                                                          ("planRef" :null))))
                         (check-and (has-json) (has-key "ref")))))
    (etask-set-context :router-id router-id :task-id task-id :key "key" :value "value")))

(defun push-a-task(&key (host "localhost") (timeout 30) (handle-time 2) (shuffle-time #'identity))
  #'(lambda(&key (router-id (get-event :router)) (queue-id (get-event :queue)))
      (tlet ((task-id (js-val "ref")
                  (tstep "Create task"
                         (tapply (http-post (list "/routers" router-id "tasks")
                                            (jsown:new-js ("callbackUrl"(format nil "http://~A:4343/task?router=~A&sleep=~A" host router-id
                                                                                (funcall shuffle-time 2)))
                                                          ("requirements" (jsown:new-js ("key" t)))
                                                          ("queueRef" queue-id)
                                                          ("userContext" (jsown:new-js ))
                                                          ("planRef" :null))))
                         (check-and (has-json) (has-key "ref")))))
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

      (let* ((q-ids (mapcar (js-val "ref")
                            (lparallel:pmapcar #'(lambda(i) (queue-new))
                                               (loop :repeat queues :collect 1)) ))
             (agent-ids (lparallel:pmapcar #'(lambda(i) (agent-new ))
                                           (loop :repeat agents :collect 1)))
             (agent-ready (lparallel:pmapcar #'(lambda(json)(agent-set :id (jsown:val json "ref") :state "ready"))
                                             agent-ids))
             (result (remove-if
                      #'second
                      (lparallel:pmapcar
                       #'(lambda(n)(funcall (funcall push  :queue-id (alexandria:random-elt q-ids))))
                       (loop :repeat tasks :collect 1) ))))
        (mapcar #'print-log result)) ) )


(defun produce-task(&key (agents 30) (queue-id) (count 30) (delay 1) )
  (let ((start-time (get-internal-real-time))
        (last-count 0))
    (setf *times* nil)
    (router-new)
    (queue-new)
    (loop :repeat agents do
       (agent-new)
       (agent-set :state "ready"))
    (loop do
         (loop for start-time = (get-internal-real-time)
            for (result status &rest rest) = (funcall  (etask-new :callback-url (format nil "http://localhost:4343/task?sleep=~A"  delay )))
            :while (and status (< (jsown:val result "queueTasks") count)) do
              (when *times*
                (let ((all (mapcar #'- *times* (rest *times*)))
                      (start-time (+ 120 (first (last *times*))))
                      (end-time   (first *times*)))
                  (format t "~%------ average =~F~%~A ~%~A count/min~%"
                          (/(length *times*)
                            (/(/ (- end-time start-time) internal-time-units-per-second) 60))
                          'all
                          (count-if #'(lambda(timestamp)(< (- (get-internal-real-time) timestamp) (* 60 internal-time-units-per-second)))
                                    *times*)))
                )
              (if (> 0 (- (/ delay agents) (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
                  (lparallel:pmapcar #'(lambda(i)(funcall  (etask-new :callback-url (format nil "http://localhost:4343/task?sleep=~A"  delay ))))
                                     (loop :repeat 2))
                  (sleep (- (/ delay agents) (/ (- (get-internal-real-time) start-time) internal-time-units-per-second) )))
              )

         (loop for (result status &rest) = (funcall (equeue-size)) :while (and status (>= (jsown:val result "size") count ))
            do
              (when *times*
                (let ((all (mapcar #'- *times* (rest *times*)))
                      (start-time (+ 120 (first (last *times*))))
                      (end-time   (first *times*)))
                  (format t "~%------ average =~F~%~A ~%~A count/min~%"
                          (/(length *times*)
                            (/(/ (- end-time start-time) internal-time-units-per-second) 60))
                          'all
                          (count-if #'(lambda(timestamp)(< (- (get-internal-real-time) timestamp) (* 60 internal-time-units-per-second)))
                                    *times*))))
              (sleep 1)
              )))
  )


(defun test-plan-and-task()
  (tlet ((router-id (js-val "ref")
                    (erouter-new))
         (default-queue-id (js-val "ref")
                   (equeue-new :router-id router-id))
         (plan-id (js-val "ref")
                  (eplan-new
                   :rules (list (jsown:new-js ("tag" :null)
                                              ("predicate" "HAS([10],#{age})")
                                              ("routes" (list  ))))
                   :default-route (jsown:new-js
                                    ("queueRef" default-queue-id))
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
  (loop for task-all = (task-all) for task = (when (listp task-all) (first task-all)) :while (and task (equal (jsown:val task "state") "completed")) do (task-del :id (jsown:val task "ref"))))

(defun setup-demo()
  (tlet((router-id (js-val "ref") (erouter-put :id "router-ivr")))

    (apply #'tand
           (append (mapcar #'(lambda(id)(equeue-put :id (first id)
                                                    :predicate (second id)
                                                    :description (third id)
                                                    :router-id router-id) )
                     '(("en-support" "HAS(#{language},'en') && #{department}=='support'" "Support in English")
                       ("es-support" "HAS(#{language},'es') && #{department}=='support'" "Support in Spanish")
                       ("en-sales" "HAS(#{language},'en') && #{department}=='sales'" "Sales in English")
                       ("es-sales" "HAS(#{language},'es') && #{department}=='sales'" "Sales in Spanish")
                       ("queue-ivr" "1==1" "Other")))
             (mapcar #'(lambda(id)(tand (eagent-put :id (first id) :address (second id)
                                                    :capabilities (jsown:new-js ("language" (third id ))
                                                                                ("department" (fourth id)))
                                                    :name (fifth id)
                                                    :router-id router-id)
                                        ;; (eagent-set :id (first id) :state "ready":address :null
                                        ;;             :capabilities :null
                                        ;;             :name :null
                                        ;;             :router-id router-id )
                                        ))
                     '(("en-es-support" "12312377880" ("en" "es") "support" "Pablo Jenkins")
                       ("en-sales" "12017621651" ("en") "sales" "John Seller")
                       ("es-sales" "12017621652" ("es") "sales" "Domingo Secada")))
             (list (eplan-put :id "simple-menu"
                         :default-queue-id "queue-ivr"
                         :queue-id :null
                         :rules '((:OBJ ("tag" . "en-sales")
                                   ("predicate" . "#{language}=='en' && #{department}=='sales'")
                                   ("routes"
                                    (:OBJ ("queueRef" . "en-sales") ("priority" . 0) ("timeout" . 3600))))
                                  (:OBJ ("tag" . "es-sales")
                                   ("predicate" . "#{language}=='es' && #{department}=='sales'")
                                   ("routes"
                                    (:OBJ ("queueRef" . "es-sales") ("priority" . 0) ("timeout" . 3600))))
                                  (:OBJ ("tag" . "en-support")
                                   ("predicate" . "#{language}=='en' && #{department}=='support'")
                                   ("routes"
                                    (:OBJ ("queueRef" . "en-support") ("priority" . 0) ("timeout" . 3600))))
                                  (:OBJ ("tag" . "es-support")
                                   ("predicate" . "#{language}=='es' && #{department}=='support'")
                                   ("routes"
                                    (:OBJ ("queueRef" . "es-support") ("priority" . 0) ("timeout" . 3600))))))) ) ) ) )

(defun create-tasks(&key (router-id (get-event :router)) (queue-id (get-event :queue)) (count 10))
  (time (remove-if #'second (lparallel:pmapcar #'(lambda(i)(funcall (etask-new :router-id router-id :queue-id queue-id))) (loop :repeat count :collect 1)))))

(defun delete-items(&key (all #'(lambda() (task-all))) 
                      (complete #'(lambda(task-id)
                                    (cond ((equal (jsown:val (task :id task-id) "state") "assigned") (funcall (etask-set :id task-id :state "completed")) )
                                          ((equal (jsown:val (task :id task-id) "state") "waiting") (funcall (etask-set :id task-id :state "canceled")) ))))
                      (delete #'(lambda(task-id) (task-del :id task-id))) )
  (loop for tasks = (funcall all) 
     :while (not(equal tasks "[]")) do
       (lparallel:pmapcar #'(lambda(task-id) ;;
                              (funcall complete task-id)
                              (funcall delete task-id))
                          (mapcar (js-val "ref") tasks))))

(defun clear-router()
  (delete-items :all #'(lambda()(router-all))
                :complete 
                #'(lambda(router-id)
                    (delete-items 
                     :all #'(lambda() (task-all :router-id router-id))
                     :complete #'(lambda(task-id)
                                   (let ((task-state (jsown:val (task :router-id router-id :id task-id) "state"))) 
                                     (cond ((equal  task-state "assigned")
                                            (funcall (etask-set :router-id router-id :id task-id :state "completed")) )
                                           ((equal task-state "waiting")
                                            (funcall (etask-set :router-id router-id  :id task-id :state "canceled")) )) ))
                     :delete #'(lambda(task-id) (funcall (etask-del :router-id router-id :id task-id)) ))
                    (delete-items 
                     :all #'(lambda() (plan-all :router-id router-id))
                     :complete #'(lambda(id))
                     :delete #'(lambda(id) (funcall (eplan-del :router-id router-id :id id)) ))
                    (delete-items 
                     :all #'(lambda() (agent-all :router-id router-id))
                     :complete #'(lambda(id))
                     :delete #'(lambda(id) (funcall (eagent-del :router-id router-id :id id)) ))
                    (delete-items 
                     :all #'(lambda() (queue-all :router-id router-id))
                     :complete #'(lambda(id))
                     :delete #'(lambda(id) (funcall (equeue-del :router-id router-id :id id)) ))
                    )
                :delete #'(lambda(id) (funcall (erouter-del :id id)) )))

(defun test-1000(&key (queues 4) (agents 8) (tasks 2000) (handle-time 0))
  #'(lambda()
      (router-new)
      (let* ((queue-ids (mapcar (js-val "ref")
                            (lparallel:pmapcar #'(lambda(i) (queue-new :predicate (format nil "#{department}=='~A'" i)))
                                               (loop for i from 0 to (1- queues) :collect i))))
             (agent-ids (mapcar (js-val "ref") (lparallel:pmapcar #'(lambda(i) (agent-new :capabilities (jsown:new-js ("department" (format nil "~A"(rem i queues))))))
                                           (loop for i from 0 to agents :collect i))))
             (rules   (mapcar #'(lambda(num queue-id) `(:OBJ ("tag" . ,(format nil "~A"num))
                                                             ("predicate" . ,(format nil "#{department}=='~A'" num))
                                                         ("routes"
                                                          (:OBJ ("queueRef" . ,queue-id) ("priority" . 0) ("timeout" . 36000)))))
                              (loop for x from 0 to queues :collect x) queue-ids))
             (plan    (plan-new
                                :queue-id (first queue-ids)
                                :default-queue-id (first queue-ids)
                                :rules rules ))
             (start-time (get-internal-real-time)))
        (time (progn
                (format t "~%Filling tasks~%")
                (time(lparallel:pmapcar 
                  #'(lambda(x)(funcall (etask-new
                                        :requirements (jsown:new-js ("department" (format nil "~A" (rem x queues))))
                                        :queue-id :null 
                                        :plan-id (jsown:val plan "ref")
                                        :callback-url (format nil "http://192.168.1.171:4343/push-task?sleep=~A" handle-time)))) 
                  (loop for x from 1 to tasks :collect x)))
                (format t "~%In queues:~A"(mapcar (compose (js-val "size") #'(lambda(qid)(queue-size :id qid)) (js-val "ref")) (queue-all)))
                (format t "~%Set agents to be ready")
                (time(lparallel:pmapcar #'(lambda(agent-id)(agent-set :capabilities :null :state "ready" :address :null :id agent-id)) agent-ids))
                (format t "~%Wait for complete")
                (loop for sizes = (print (list* (queues:qsize *queue*) (mapcar (compose (js-val "size") #'(lambda(qid)(queue-size :id qid)) (js-val "ref")) (queue-all)))) :until (every #'zerop sizes) do 
                     (loop for size = (queues:qsize *queue*) :while (> size 0) do
                          (time (remove-if-not (compose #'null #'second) (lparallel:pmapcar #'(lambda(i) (destructuring-bind (router-id task-id delay start) (queues:qpop *queue*) (autocomplete-task router-id task-id))) (loop for x from 1 to (print size) :collect x))) ))
                     ) )) )))
(defun handle-tasks()
  )

(defun test-performance (&key (tasks 10)(queues 1) (agents 1) (handle-time 0))
  #'(lambda()
      (router-new)
      (let* ((q-ids (mapcar (js-val "ref")
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
             (agent-ready (time (lparallel:pmapcar #'(lambda(json)(agent-set :id (jsown:val json "ref") :state "ready"))
                                                   agent-ids))) )
        (time (loop for queue = (lparallel:pevery #'(lambda(id) (zerop (jsown:val (queue-size :id id) "size")))
                                                  (mapcar (js-val "ref") (queue-all)))
                 :while (not queue)  do (sleep 1)))
        (mapcar #'print-log result)) ) )

;;(loop for a from 1 to 1 for start-time = (get-internal-real-time) for res = (funcall (test-performance :tasks 1000 :queues 25 :agents 1 :handle-time 1)) :collect (floor (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))

(defun complete-tasks(&key (try 3))
  (tlet ((router-id (js-val "ref")(erouter-new))
         (queue-id (js-val "ref") (equeue-new)))
    (let ((res(lparallel:pmapcar #'funcall
                                 (loop :repeat try :append (list (etask-new :callback-url "http://localhost:4343/task?sleep=0" :router-id router-id :queue-id queue-id)
                                                                (tlet ((agent-id (eagent-new)))
                                                                  (eagent-set :state "ready")))))))
      (tstep-result (format nil "run in parallel") res (not(some #'null (mapcar #'second res)))  "run in parallel" nil (reduce #'append res)))))

(defun test-complete-and-create-queue()
  (time(test-random :prefix '("create router"
                              "create queue"  "create agent" "create task when there are no ready agents"
                              "set-agent ready if there are waiting tasks" "create queue"
                              "check agent state"))))

(defun dump-time (f &optional (text "time")) 
  #'(lambda()
      (let ((start-time (get-internal-real-time)))
        (prog1 (funcall f)
          (format t "~%~A:~F" text (/ (- (get-internal-real-time) start-time) internal-time-units-per-second))))))

(defun time-to-complete-task()
  (tlet((router (js-val "ref") (erouter-new))
        (queue  (js-val "ref") (equeue-new :router-id router))
        (agent (js-val "ref") (eagent-new :router-id router))
        (task (js-val "ref")  (etask-new :router-id router :queue-id queue)))
    (tand (eagent-set :router-id router :id agent :state "ready")
          (dump-time (twait (eagent :router-id router :id agent :checks (has-kv "state" "busy"))
                       :timeout 200)))))

(defun test-plan-task(&key (predicate "true") (requirements (jsown:new-js )))
  (tlet ((router (js-val "ref") (erouter-new))
         (default-queue  (js-val "ref") (equeue-new :router-id router :description "default queue" :predicate "false"))
         (queue  (js-val "ref") (equeue-new :router-id router :description "destination" :predicate "false"))
         (plan (js-val "ref") (eplan-new :router-id router :predicate predicate :default-queue-id default-queue :queue-id queue))
         (task  (js-val "ref") (etask-new :router-id router :plan-id plan :queue-id :null :requirements requirements)))
    (etask :router-id router :id task :checks (contains queue))))

(defun test-plan-task-fixed(router-name &key (predicate "true") (requirements (jsown:new-js )))
  (tlet ((router (js-val "ref") (erouter-put :id router-name))
         (default-queue  (js-val "ref") (equeue-put :router-id router :id "default-queue" :description "default queue" :predicate "false"))
         (queue  (js-val "ref") (equeue-put :router-id router :id "destination-queue" :description "destination" :predicate "false"))
         (plan (js-val "ref") (eplan-put :router-id router :id "simple-plan" :predicate predicate :default-queue-id default-queue :queue-id queue))
         (task  (js-val "ref") (etask-new :router-id router :plan-id plan :queue-id :null :requirements requirements)))
    (etask :router-id router :id task :checks (contains queue))))


(defun match-agent(&key (predicate "true") (capabilities (jsown:new-js ("language" "en"))))
  (tlet ((router (js-val "ref") (erouter-new))
         (queue  (js-val "ref") (equeue-new :router-id router :description "destination" :predicate predicate))
         (agent (js-val "ref") (eagent-new :router-id router :capabilities capabilities)))
    (eagent :router-id router :id agent :checks (contains queue))))
