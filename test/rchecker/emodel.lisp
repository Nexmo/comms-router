(in-package :rchecker)
(defun action(name precondition action update-model)
  (list precondition
        #'(lambda(model)
            (let* ((res (funcall(funcall action model)))
                   (new-model (if (second res)
                                  (funcall update-model res (copy-tree (jsown:extend-js model
                                                                         ("result" (second res)))))
                                  model)))
              (format t "~%~A executed - ~S" name res)
              (list res new-model)))
        name))

(defun select-actions(index list)
  (list (action (format nil "select last of ~A" list)
                (fand (fhas-key list)
                      (fhas-kv list 0 #'(lambda(js-val val)(> (length js-val) val))))
                #'(lambda(model)(tstep-result (format nil "Select the last of ~A." list) 0 t (format nil "Select the last of ~A." list) nil))
                #'(lambda(res model)(jsown:extend-js (copy-tree model) (index 0))))

        (action (format nil "select previous of ~A" list)
                (fand (fhas-key list)
                      (fhas-key index)
                      (fcompare-keys index list #'(lambda(selected items)(< (1+ selected) (length items)))))
                (mlet((selected (js-val index)))
                  (tstep-result (format nil "Select previous of ~A" list)  (1+ selected) t
                                (format nil "Select previous of ~A" list) nil) )
                #'(lambda(res model)(let* ((next (1+ (jsown:val model index))))
                                      (jsown:extend-js (copy-tree model) (index next)))))))

(defparameter *tasks*
  (append

   ;; ;; queues
   ;; (action "create queue"
   ;;         (for
   ;;          (fnot (fhas-key "queues"))
   ;;          (fand (fhas-kv "queues" 2 #'(lambda(js-val val)(< (length js-val) val)))))
   ;;         #'(lambda(model)(equeue-new :router-id (jsown:val model "router")
   ;;                                    :queue-id (jsown:val model "queue")))
   ;;         #'(lambda(res model)
   ;;             (jsown:extend-js (copy-tree model)
   ;;               ("queues" (list* (jsown:new-js ("id" (jsown:val (first res) "id"))) ;;
   ;;                               (when (jsown:keyp model "queues")
   ;;                                 (jsown:val model "queues")))))))

   ;; (action "select last queue"
   ;;         (fand (fhas-key "queues")
   ;;               (fhas-kv "queues" 0 #'(lambda(js-val val)(> (length js-val) val))))
   ;;         #'(lambda(model)(tstep-result "Select the latest queue." 0 t "Select the latest queue." nil))
   ;;         #'(lambda(res model)(jsown:extend-js (copy-tree model) ("selected-queue" 0))))

   ;; (action "select next queue"
   ;;         (fand (fhas-key "queues")
   ;;               (fhas-key "selected-queue")
   ;;               (fcompare-keys "selected-queue" "queues" #'(lambda(selected items)(< (1+ selected) (length items)))))
   ;;         #'(lambda(model)(tstep-result "Select previous queue" (1+ (jsown:val model "selected-queue")) t "Select previous queue" nil))
   ;;         #'(lambda(res model)(let* ((next (1+ (jsown:val model "selected-queue"))))
   ;;                               (jsown:extend-js (copy-tree model) ("selected-queue" next)))))

   ;; (action "delete queue"
   ;;         (fand
   ;;          (fhas-key "selected-queue"))
   ;;         #'(lambda(model)(let* ((selected (jsown:val model "selected-queue"))
   ;;                                (queues (jsown:val model "queues"))
   ;;                                (queue (nth selected queues)))
   ;;                           (equeue-del :id (jsown:val queue "id") :router-id (jsown:val model "router"))))
   ;;         #'(lambda(res model)(let* ((selected (jsown:val model "selected-queue"))
   ;;                                    (queues (jsown:val model "queues"))
   ;;                                    (queue (nth selected queues)))
   ;;                               (jsown:remkey (jsown:extend-js(copy-tree model)
   ;;                                               ("queues" (remove-nth selected (copy-tree queues))))
   ;;                                             "selected-queue"))))

   ;; ;; plans
   ;; (action "create plan"
   ;;         (fand
   ;;          (fhas-key "selected-queue")
   ;;          (for
   ;;           (fnot (fhas-key "plans"))
   ;;           (fand (fhas-kv "plans" 2 #'(lambda(js-val val)(< (length js-val) val))))))
   ;;         #'(lambda(model)(let((selected (jsown:val model "selected-queue"))
   ;;                              (queues (jsown:val model "queues"))
   ;;                              (queue (nth selected queues)))
   ;;                             (eplan-new :router-id (jsown:val model "router")
   ;;                                        :queue-id (jsown:val queue "id"))))
   ;;         #'(lambda(res model)
   ;;             (jsown:extend-js (copy-tree model)
   ;;               ("plans" (list* (jsown:new-js ("id" (jsown:val (first res) "id"))) ;;
   ;;                               (when (jsown:keyp model "plans")
   ;;                                 (jsown:val model "plans")))))))

   ;; (action "select last plan"
   ;;         (fand (fhas-key "plans")
   ;;               (fhas-kv "plans" 0 #'(lambda(js-val val)(> (length js-val) val))))
   ;;         #'(lambda(model)(tstep-result "Select the latest plan." 0 t "Select the latest plan." nil))
   ;;         #'(lambda(res model)(jsown:extend-js (copy-tree model) ("selected-plan" 0))))

   ;; (action "select next plan"
   ;;         (fand (fhas-key "plans")
   ;;               (fhas-key "selected-plan")
   ;;               (fcompare-keys "selected-plan" "plans" #'(lambda(selected items)(< (1+ selected) (length items)))))
   ;;         #'(lambda(model)(tstep-result "Select previous plan" (1+ (jsown:val model "selected-plan")) t "Select previous plan" nil))
   ;;         #'(lambda(res model)(let* ((next (1+ (jsown:val model "selected-plan"))))
   ;;                               (jsown:extend-js (copy-tree model) ("selected-plan" next)))))

   ;; (action "delete plan"
   ;;         (fand
   ;;          (fhas-key "selected-plan"))
   ;;         #'(lambda(model)(let* ((selected (jsown:val model "selected-plan"))
   ;;                                (plans (jsown:val model "plans"))
   ;;                                (plan (nth selected plans)))
   ;;                           (eplan-del :id (jsown:val plan "id") :router-id (jsown:val model "router"))))
   ;;         #'(lambda(res model)(let* ((selected (jsown:val model "selected-plan"))
   ;;                                    (plans (jsown:val model "plans"))
   ;;                                    (plan (nth selected plans)))
   ;;                               (jsown:remkey (jsown:extend-js(copy-tree model)
   ;;                                               ("plans" (remove-nth selected (copy-tree plans))))
   ;;                                             "selected-plan"))))


   ;;agents
   (list (action "create agent"
           (for
            (fnot (fhas-key "agents"))
            (fand (fhas-kv "agents" 2 #'(lambda(js-val val)(< (length js-val) val)))))
           (mlet ((router-id (js-val "router")))
             (eagent-new :router-id router-id))
           #'(lambda(res model)
               (jsown:extend-js (copy-tree model)
                 ("agents" (list* (jsown:extend-js (jsown:new-js ("id" (jsown:val (first res) "id")))
                                    ("state" "offline")
                                    ("lastTimeAtBusyState" (get-internal-real-time))) ;;
                                  (when (jsown:keyp model "agents")
                                    (jsown:val model "agents")))))))



   (action "set-agent ready if there are waiting tasks"
           (fand
            (fand (fhas-key "tasks")
                  #'(lambda(model) (some (fhas-kv "state" "waiting" #'equal)
                                         (jsown:val model"tasks"))))
            (fhas-key "selected-agent")
            (fnth "selected-agent" "agents"
                  (fnot (for
                         (fhas-kv "state" "ready" #'equal)
                         (fhas-kv "state" "busy" #'equal)))))
           (mlet((agent-id (js-val "id") (js-selected "selected-agent" "agents"))
                 (router-id (js-val "router")))
             (eagent-set :id agent-id :state "ready"
                         :router-id router-id))

           #'(lambda(res model)
               (let* ((selected (jsown:val model "selected-agent"))
                      (agents (jsown:val model "agents"))
                      (tasks (jsown:val model "tasks"))
                      (agent (nth selected agents))
                      (task-position (position-if (fhas-kv "state" "waiting" #'equal) tasks :from-end t))
                      (task (nth task-position tasks)))
                 (jsown:extend-js (copy-tree model)
                   ("agents" (set-nth selected (copy-tree agents)
                                      (jsown::extend-js agent
                                        ("state" "busy") ) ))
                   ("tasks" (set-nth task-position (copy-tree tasks)
                                     (jsown::extend-js task
                                       ("state" "assigned")
                                       ("agent-id" (jsown:val agent "id")))))))))

   (action "set-agent ready if there are no waiting tasks"
           (fand
            (fhas-key "selected-agent")
            (fnth "selected-agent" "agents"
                  (fnot (for
                         (fhas-kv "state" "ready" #'equal)
                         (fhas-kv "state" "busy" #'equal))))
            (for
             (fnot(fhas-key "tasks"))
             (fand
              (fand (fhas-key "tasks")
                    #'(lambda(model) (not(some (fhas-kv "state" "waiting" #'equal)
                                               (jsown:val model"tasks"))))))))
           (mlet ((agent-id (js-val "id") (js-selected "selected-agent" "agents"))
                  (router-id (js-val "router")))
             (eagent-set :id agent-id :state "ready"
                         :router-id router-id))
           #'(lambda(res model)
               (let* ((selected (jsown:val model "selected-agent"))
                      (agents (jsown:val model "agents"))
                      (agent (nth selected agents)) )
                 (jsown:extend-js (copy-tree model)
                   ("agents" (set-nth selected (copy-tree agents)
                                      (jsown::extend-js agent
                                        ("state" "ready")
                                        ("lastTimeAtBusyState" (get-internal-real-time)))))))))

   (action "check agent state"
           (fhas-key "selected-agent")
           (mlet((agent-id (js-val "id") (js-selected "selected-agent" "agents"))
                 (agent-state (js-val "state") (js-selected "selected-agent" "agents"))
                 (router-id (js-val "router")))
             (twait
              (eagent :id agent-id
                      :checks (has-kv "state" agent-state)
                      :router-id router-id)
              :timeout 10))
           #'(lambda(res model)model))

   (action "delete agent"
           (fand
            (fhas-key "selected-agent")
            (fnth "selected-agent" "agents"
                  (fnot (fhas-kv "state" "busy" #'equal))))
           (mlet((agent-id (js-val "id") (js-selected "selected-agent" "agents"))
                 (router-id (js-val "router")))
             (twait (eagent-del :id agent-id :router-id router-id)
                    :timeout 10))
           #'(lambda(res model)(let* ((selected (jsown:val model "selected-agent"))
                                      (agents (jsown:val model "agents"))
                                      (agent (nth selected agents)))
                                 (jsown:remkey (jsown:extend-js(copy-tree model)
                                                 ("agents" (remove-nth selected (copy-tree agents))))
                                               "selected-agent")))))
   (select-actions "selected-agent" "agents")

   ;; tasks
   (list (action
    "create task when there are no ready agents"
    (fand
     (for
      (fnot (fhas-key "tasks"))
      (fand (fhas-kv "tasks" 5 #'(lambda(js-val val)(< (length js-val) val)))))
     (for
      (fnot (fhas-key "agents"))
      (fand (fhas-key "agents") #'(lambda(model) (every #'(lambda(task)(not (equal (jsown:val task "state") "ready")))
                                                        (jsown:val model "agents"))))))
    (mlet ((router-id (js-val "router"))
           (queue-id (js-val "queue")))
      (let((waiting-tasks (if (jsown:keyp model "tasks")
                              (length (remove-if-not
                                       (fhas-kv "state" "waiting" #'equal)
                                       (jsown:val model "tasks")))
                              0)))
        (tand (twait (equeue-size :id queue-id
                                  :description (format nil "There should be ~A tasks in the queue" waiting-tasks)
                                  :checks (check-and (has-json) (has-key "size")
                                                     (has-kv "size" waiting-tasks))
                                  :router-id router-id))
              (etask-new :checks (check-and (has-json) (has-key "id")
                                            (has-kv "queueTasks" waiting-tasks))
                         :router-id router-id
                         :queue-id queue-id)) ) )
    #'(lambda(res model)(let* ((waiting-tasks (if (jsown:keyp model "tasks")
                                              (length (remove-if-not
                                                       (fhas-kv "state" "waiting" #'equal)
                                                       (jsown:val model "tasks")))
                                              0)) )
                          (jsown:extend-js model
                            ("tasks" (list* (jsown:extend-js
                                                (jsown:new-js ("id" (jsown:val (first res) "id")))
                                              ("state" "waiting") )
                                            (when (jsown:keyp model "tasks")
                                              (jsown:val model "tasks"))))))))

   (action
    "create task when there are ready agents"
    (fand
     (for
      (fnot (fhas-key "tasks"))
      (fand (fhas-kv "tasks" 5 #'(lambda(js-val val)(< (length js-val) val)))))
     (fand (fhas-key "agents") #'(lambda(model) (some (fhas-kv "state" "ready" #'equal)
                                                      (jsown:val model "agents")))))
    (mlet ((router-id (js-val "router"))
           (queue-id (js-val "queue"))
           (agents (js-val "agents")))
      (let*((waiting-tasks (if (jsown:keyp model "tasks")
                               (length (remove-if-not
                                        (fhas-kv "state" "waiting"
                                                 #'equal)
                                        (jsown:val model "tasks")))
                               0))
            (agent (find-if (fhas-kv "state" "ready" #'equal)
                            (sort  (copy-tree agents) #'> :key #'(lambda(agent)(jsown:val agent "lastTimeAtBusyState"))) ))
            (agent-pos (position-if (fhas-kv "id" (jsown:val agent "id") #'equal) agents)))
        (tand
         (twait (equeue-size :id queue-id
                             :description (format nil "There should be ~A tasks in the queue" waiting-tasks)
                             :checks (check-and (has-json) (has-key "size")
                                                (has-kv "size" waiting-tasks))
                             :router-id router-id ))
         (etask-new :checks (check-and (has-json) (has-key "id")
                                       (has-kv "queueTasks" waiting-tasks))
                    :router-id router-id
                    :queue-id queue-id)) ))

    #'(lambda(res model)
        (let* ((agents (jsown:val model "agents"))
               (agent (find-if (fhas-kv "state" "ready" #'equal)
                               (sort  (copy-tree agents) #'> :key #'(lambda(agent)(jsown:val agent "lastTimeAtBusyState"))) ))
               (agent-pos (position-if (fhas-kv "id" (jsown:val agent "id") #'equal) agents)))
          (jsown:extend-js (copy-tree model)
            ("tasks" (list* (jsown:extend-js
                                (jsown:new-js ("id" (jsown:val (first res) "id")))
                              ("state" "assigned")
                              ("agent-id" (jsown:val agent "id")))
                            (when (jsown:keyp model "tasks")
                              (jsown:val model "tasks"))))
            ("agents" (set-nth agent-pos (copy-tree agents)
                               (jsown::extend-js agent ("state" "busy")) ))))))

   (action
    "check task state"
    (fhas-key "selected-task")
    (mlet ((router-id (js-val "router"))
           (task (js-selected "selected-task" "tasks")))
      (twait (etask :id (jsown:val task "id")
                    :state (jsown:val task "state")
                    :router-id router-id)
             :timeout 10))
    #'(lambda(res model)model))

   (action
    "complete task when there are no waiting tasks"
    (fand
     (fhas-key "selected-task")
     (fnth "selected-task" "tasks" (fhas-kv "state" "assigned" #'equal))
     #'(lambda(model) (every (fnot (fhas-kv "state" "waiting" #'equal))
                             (jsown:val model"tasks"))))
    (mlet ((task-id (js-val "id") (js-selected "selected-task" "tasks"))
           (router-id (js-val "router")))
      (etask-set :id task-id :state "completed"
                 :router-id router-id) )
    #'(lambda(res model)(let* ((selected (jsown:val model "selected-task"))
                           (tasks (jsown:val model "tasks"))
                           (task (nth selected tasks))
                           (agent-id (jsown:val task "agent-id"))
                           (agents (jsown:val model "agents"))
                           (agent-pos (position-if #'(lambda(agent) (equal (jsown:val agent "id") agent-id))
                                                   agents))
                           (agent (nth agent-pos agents)))
                      (jsown:extend-js (copy-tree model)
                        ("agents" (set-nth agent-pos (copy-tree agents)
                                           (jsown::extend-js agent
                                             ("state" "ready")
                                             ("lastTimeAtBusyState" (get-internal-real-time)))
                                           ))
                        ("tasks" (set-nth selected (copy-tree tasks)
                                          (jsown::extend-js task ("state" "completed")
                                                            ("agent-id" ()))))))))

   (action
    "complete task when there are waiting tasks"
    (fand
     (fhas-key "selected-task")
     (fnth "selected-task" "tasks" (fhas-kv "state" "assigned" #'equal))
     #'(lambda(model) (some (fhas-kv "state" "waiting" #'equal)
                            (jsown:val model"tasks"))))

    (mlet ((task-id (js-val "id") (js-selected "selected-task" "tasks"))
           (router-id (js-val "router")))
      (etask-set :id task-id :state "completed"
                 :router-id router-id) )
    #'(lambda(res model)(let* ((selected (jsown:val model "selected-task"))
                               (tasks (jsown:val model "tasks"))
                               (task (nth selected tasks))
                               (task-position (position-if (fhas-kv "state" "waiting" #'equal) tasks :from-end t))
                               (next-task (nth task-position tasks))
                               (agent-id (jsown:val task "agent-id"))
                               (agents (jsown:val model "agents"))
                               (agent-pos (position-if #'(lambda(agent) (equal (jsown:val agent "id") agent-id))
                                                       agents))
                               (agent (nth agent-pos agents)))
                          (jsown:extend-js model
                            ("agents" (set-nth agent-pos (copy-tree agents)
                                               (jsown::extend-js agent
                                                 ("state" "busy")
                                                 ("lastTimeAtBusyState" (get-internal-real-time)))))
                            ("tasks" (set-nth task-position
                                              (set-nth selected (copy-tree tasks)
                                                       (jsown::extend-js task ("state" "completed")
                                                                         ("agent-id" ())))
                                              (jsown::extend-js next-task ("state" "assigned")
                                                                ("agent-id" (jsown:val agent "id")))))))))

   (action
    "delete task"
    (fand
     (fhas-key "selected-task")
     (fnth "selected-task" "tasks" (for(fhas-kv "state" "completed" #'equal)
                                       (fhas-kv "state" "waiting" #'equal))) )
    (mlet ((task-id (js-val "id") (js-selected "selected-task" "tasks"))
           (router-id (js-val "router")))
      (etask-del :id task-id
                 :router-id router-id) )

    #'(lambda(res model) (let* ((selected (jsown:val model "selected-task"))
                                (tasks (jsown:val model "tasks"))
                                (task (nth selected tasks)))
                           (jsown:remkey (jsown:extend-js(copy-tree model)
                                           ("tasks" (remove-nth selected (copy-tree tasks))))
                                         "selected-task")))))
   (select-actions "selected-task" "tasks")
   ))
