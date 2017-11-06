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

(defparameter *tasks*
  (list
   (action "create agent"
           (for
            (fnot (fhas-key "agents"))
            (fand (fhas-kv "agents" 2 #'(lambda(js-val val)(< (length js-val) val)))))
           #'(lambda(model)(eagent-new :router-id (jsown:val model "router")))
           #'(lambda(res model)
               (jsown:extend-js (copy-tree model)
                 ("agents" (list* (jsown:extend-js (jsown:new-js ("id" (ref-new "agent" :value (jsown:val (first res) "id"))))
                                    ("state" "offline")
                                    ("lastTimeAtBusyState" (ref-new "agent-time" :value (get-internal-real-time)))) ;;
                                  (when (jsown:keyp model "agents")
                                    (jsown:val model "agents")))))))

   (action "select last agent"
           (fand (fhas-key "agents")
                 (fhas-kv "agents" 0 #'(lambda(js-val val)(> (length js-val) val))))
           #'(lambda(model)(tstep-result "Select the latest agent." 0 t "Select the latest agent." nil))
           #'(lambda(res model)(jsown:extend-js (copy-tree model) ("selected-agent" 0))))

   (action "select next agent"
           (fand (fhas-key "agents")
                 (fhas-key "selected-agent")
                 (fcompare-keys "selected-agent" "agents" #'(lambda(selected items)(< (1+ selected) (length items)))))
           #'(lambda(model)(tstep-result "Select previous agent" (1+ (jsown:val model "selected-agent")) t "Select previous agent" nil))
           #'(lambda(res model)(let* ((next (1+ (jsown:val model "selected-agent"))))
                                 (jsown:extend-js (copy-tree model) ("selected-agent" next)))))

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
           #'(lambda(model)
               (let* ((selected (jsown:val model "selected-agent"))
                      (agents (jsown:val model "agents"))
                      (agent (nth selected agents)))
                 (eagent-set :router-id (jsown:val model "router")
                             :id (ref-id "agent" agent) :state "ready")))
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
           #'(lambda(model)
               (let* ((selected (jsown:val model "selected-agent"))
                      (agents (jsown:val model "agents"))
                      (agent (nth selected agents)))
                 (eagent-set :id (ref-id "agent" agent)
                             :state "ready"
                             :router-id (jsown:val model "router"))))
           #'(lambda(res model)
               (let* ((selected (jsown:val model "selected-agent"))
                      (agents (jsown:val model "agents"))
                      (agent (nth selected agents)) )
                 (jsown:extend-js (copy-tree model)
                   ("agents" (set-nth selected (copy-tree agents)
                                      (jsown::extend-js agent
                                        ("state" "ready")
                                        ("lastTimeAtBusyState" (ref-set "agent-time" agent (get-internal-real-time))))))))))

   (action "check agent state"
           (fhas-key "selected-agent")
           #'(lambda(model)(let* ((selected (jsown:val model "selected-agent"))
                                  (agents (jsown:val model "agents"))
                                  (agent (nth selected agents)))
                             (twait
                              (eagent :id (ref-id "agent" agent)
                                      :checks (has-kv "state" (jsown:val agent "state"))
                                      :router-id (jsown:val model "router"))
                              :timeout 10)))
           #'(lambda(res model)model))

   (action "delete agent"
           (fand
            (fhas-key "selected-agent")
            (fnth "selected-agent" "agents"
                  (fnot (fhas-kv "state" "busy" #'equal))))
           #'(lambda(model)(let* ((selected (jsown:val model "selected-agent"))
                                  (agents (jsown:val model "agents"))
                                  (agent (nth selected agents)))
                             (twait (eagent-del :id (ref-id "agent" agent) :router-id (jsown:val model "router"))
                                    :timeout 10)))
           #'(lambda(res model)(let* ((selected (jsown:val model "selected-agent"))
                                      (agents (jsown:val model "agents"))
                                      (agent (nth selected agents)))
                                 (ref-del "agent" (jsown:val agent "id"))
                                 (ref-del "agent-time" (jsown:val agent "id"))
                                 (jsown:remkey (jsown:extend-js(copy-tree model)
                                                 ("agents" (remove-nth selected (copy-tree agents))))
                                               "selected-agent"))))

   ;; tasks
   (action
    "create task when there are no ready agents"
    (fand
     (for
      (fnot (fhas-key "tasks"))
      (fand (fhas-kv "tasks" 5 #'(lambda(js-val val)(< (length js-val) val)))))
     (for
      (fnot (fhas-key "agents"))
      (fand (fhas-key "agents") #'(lambda(model) (every #'(lambda(task)(not (equal (jsown:val task "state") "ready")))
                                                        (jsown:val model "agents")))) ))
    #'(lambda(model)(let* ((waiting-tasks (if (jsown:keyp model "tasks")
                                              (length (remove-if-not
                                                       (fhas-kv "state" "waiting" #'equal)
                                                       (jsown:val model "tasks")))
                                              0)))
                      (tand (twait (equeue-size :router-id (jsown:val model "router")
                                                :id (jsown:val model "queue")
                                                :description (format nil "There should be ~A tasks in the queue" waiting-tasks)
                                                :checks (check-and (has-json) (has-key "size")
                                                                   (has-kv "size" waiting-tasks))))
                            (etask-new :checks (check-and (has-json) (has-key "id")
                                                          (has-kv "queueTasks" waiting-tasks))
                                       :router-id (jsown:val model "router")
                                       :queue-id (jsown:val model "queue")))))
    #'(lambda(res model)(let* ((waiting-tasks (if (jsown:keyp model "tasks")
                                              (length (remove-if-not
                                                       (fhas-kv "state" "waiting" #'equal)
                                                       (jsown:val model "tasks")))
                                              0)) )
                          (jsown:extend-js model
                            ("tasks" (list* (jsown:extend-js
                                                (jsown:new-js ("id" (ref-new "task" :value (jsown:val (first res) "id"))))
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
    #'(lambda(model)
        (let* ((waiting-tasks (if (jsown:keyp model "tasks")
                                  (length (remove-if-not
                                           (fhas-kv "state" "waiting"
                                                    #'equal)
                                           (jsown:val model "tasks")))
                                  0))
               (agents (jsown:val model "agents"))
               (agent (find-if (fhas-kv "state" "ready" #'equal)
                               (sort  (copy-tree agents) #'> :key #'(lambda(agent)(ref-id "agent-time" agent))) ))
               (agent-pos (position-if (fhas-kv "id" (jsown:val agent "id") #'equal) agents)))
          (tand
           (twait (equeue-size
                   :description (format nil "There should be ~A tasks in the queue" waiting-tasks)
                   :checks (check-and (has-json) (has-key "size")
                                      (has-kv "size" waiting-tasks))
                   :router-id (jsown:val model "router")
                   :id (jsown:val model "queue")))
           (etask-new :checks (check-and (has-json) (has-key "id")
                                         (has-kv "queueTasks" waiting-tasks))
                      :router-id (jsown:val model "router")
                      :queue-id (jsown:val model "queue")))))
    #'(lambda(res model)
        (let* ((agents (jsown:val model "agents"))
               (agent (find-if (fhas-kv "state" "ready" #'equal)
                               (sort  (copy-tree agents) #'> :key #'(lambda(agent)(ref-id "agent-time" agent))) ))
               (agent-pos (position-if (fhas-kv "id" (jsown:val agent "id") #'equal) agents)))
          (jsown:extend-js (copy-tree model)
            ("tasks" (list* (jsown:extend-js
                                (jsown:new-js ("id" (ref-new "task" :value (jsown:val (first res) "id"))))
                              ("state" "assigned")
                              ("agent-id" (jsown:val agent "id")))
                            (when (jsown:keyp model "tasks")
                              (jsown:val model "tasks"))))
            ("agents" (set-nth agent-pos (copy-tree agents)
                               (jsown::extend-js agent ("state" "busy")) ))))))

   (action
    "select last task"
    (fand (fhas-key "tasks")
          (fhas-kv "tasks" 0 #'(lambda(js-val val)(> (length js-val) val))))
    #'(lambda(model)(tstep-result "Select the latest task." 0 t "Select the latest task." nil))
    #'(lambda(res model)(jsown:extend-js (copy-tree model) ("selected-task" 0))))

   (action
    "select next task"
    (fand (fhas-key "tasks")
          (fhas-key "selected-task")
          (fcompare-keys "selected-task" "tasks" #'(lambda(selected tasks)(< (1+ selected) (length tasks)))))
    #'(lambda(model)(let* ((next (1+ (jsown:val model "selected-task"))))
                      (tstep-result "Select previous task." next t "Select the previous task." nil)))
    #'(lambda(res model)(let* ((next (1+ (jsown:val model "selected-task"))))
                          (jsown:extend-js (copy-tree model) ("selected-task" next)))))

   (action
    "check task state"
    (fhas-key "selected-task")
    #'(lambda(model)(let* ((selected (jsown:val model "selected-task"))
                           (tasks (jsown:val model "tasks"))
                           (task (nth selected tasks)))
                      (twait (etask :id (ref-id "task" task)
                                    :state (jsown:val task "state")
                                    :router-id (jsown:val model "router"))
                             :timeout 10)))
    #'(lambda(res model)model))

   (action
    "complete task when there are no waiting tasks"
    (fand
     (fhas-key "selected-task")
     (fnth "selected-task" "tasks" (fhas-kv "state" "assigned" #'equal))
     #'(lambda(model) (every (fnot (fhas-kv "state" "waiting" #'equal))
                             (jsown:val model"tasks"))))
    #'(lambda(model)(let* ((selected (jsown:val model "selected-task"))
                           (tasks (jsown:val model "tasks"))
                           (task (nth selected tasks)))
                      (etask-set :id (ref-id "task" task) :state "completed"
                                 :router-id (jsown:val model "router"))))

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
                                             ("lastTimeAtBusyState" (ref-new "agent-time" :value (get-internal-real-time))))
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
    #'(lambda(model)(let* ((selected (jsown:val model "selected-task"))
                           (tasks (jsown:val model "tasks"))
                           (task (nth selected tasks)))
                      (etask-set :id (ref-id "task" task) :state "completed"
                                 :router-id (jsown:val model "router"))))
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
                                                 ("lastTimeAtBusyState" (ref-new "agent-time" :value (get-internal-real-time))))))
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
    #'(lambda(model)(let* ((selected (jsown:val model "selected-task"))
                           (tasks (jsown:val model "tasks"))
                           (task (nth selected tasks)))
                      (etask-del :id (ref-id "task" task)
                                 :router-id (jsown:val model "router"))))
    #'(lambda(res model) (let* ((selected (jsown:val model "selected-task"))
                                (tasks (jsown:val model "tasks"))
                                (task (nth selected tasks)))
                           (ref-del "task" (jsown:val task "id"))
                           (jsown:remkey (jsown:extend-js(copy-tree model)
                                           ("tasks" (remove-nth selected (copy-tree tasks))))
                                         "selected-task"))))))
