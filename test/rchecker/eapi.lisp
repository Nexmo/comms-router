(in-package :rchecker)

(defun tapply(request)
  #'(lambda()
      (list (apply (funcall *endpoint* #'cmd-curl) (funcall request))
            (apply (funcall *endpoint* #'transport) (funcall request)))))

(defun etask(&key (router-id (get-event :router)) (task-id (get-event :task)) (state "assigned"))
  (tstep (format nil "Check that task is in state ~A." state)
         (tapply (http-get "/routers" router-id "tasks" task-id ))
         (check-and (has-json) (has-kv "state" state))))

(defun etask-set(&key (router-id (get-event :router)) (task-id (get-event :task)) (state :null) )
  (tstep (format nil "Set task's state = ~A." state )
         (tapply (http-post (list "/routers" router-id "tasks" task-id)
                            (jsown:new-js ("state" "completed")
                                          )))
         (is-equal "")))

(defun etask-all(&key (router-id (get-event :router)))
  (tstep "List all tasks"
         (tapply (http-get "/routers" router-id "tasks"))
         (not-contains "error")))

(defun etask-set-context(&key (router-id (get-event :router)) (task-id (get-event :task)) (key "key") (value "value"))
  (tstep (format nil "Set task's context~A = ~A." key value)
         (tapply (http-put (list "/routers" router-id "tasks" task-id "user_context" key)
                            value))
         (is-equal "")))

;;; agent
(defun eagent-all(&key (router-id (get-event :router)))
  (tstep "List all agents"
         (tapply (http-get "/routers" router-id "agents"))
         (not-contains "error")))
(defun eagent-del(&key (router-id (get-event :router))
                    (agent-id (get-event :agent)))
  (tstep (format nil "Delete agent with id ~A." agent-id)
         (tapply (http-del "/routers" router-id "agents" agent-id))
         (is-equal "")))
