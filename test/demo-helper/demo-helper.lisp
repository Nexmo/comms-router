;;;; demo-helper.lisp

(in-package #:demo-helper)
(hunchentoot:define-easy-handler (set-state :uri "/set-state") (agent state)
  (setf (hunchentoot:content-type*) "text/html")
  (rchecker::agent-set :router-id "router-ivr" :id agent :address :null :capabilities :null :state state)
  (hunchentoot:redirect "/") )

(hunchentoot:define-easy-handler (app :uri "/") (agent state)
  (setf (hunchentoot:content-type*) "text/html")

  (let* ((agents (rchecker::agent-all :router-id "router-ivr"))
         (queues (rchecker::queue-all :router-id "router-ivr"))
         (qtasks (mapcar #'(lambda(id) (rchecker::queue-tasks :id id :router-id "router-ivr" ))
                         (mapcar (rchecker::js-val "ref") queues))))
    (when (and agent state)
      (funcall (rchecker::eagent-set :router-id "router-ivr" :id agent :address :null :capabilities :null :state state)))
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:meta :http-equiv "refresh" :content 3)
      (:html
       (:head (:title "Comms router demo"))
       (:body
        (:h1 "Comms router demo")
        (:h2 "Agents:")
        (:table
         (:tr (:th "id") (:th "state") (:th "toggle"))
         (loop for agent in agents do
              (htm (:tr (:td (str (jsown:val agent "ref")) )
                        (:td  (str (jsown:val agent "state")))
                        (:td (:a :href
                                 (string
                                  (format nil "?agent=~A&state=~A"
                                          (jsown:val agent "ref")
                                          (if (equal (jsown:val agent "state") "ready") "offline" "ready")))
                                 "toggle")))  ) ) )
        (:h2 "Queues:")
        (:table
         (:tr (:th "id") (:th "predicate") (:th "size"))
         (loop for queue in queues
            for tasks in qtasks do
              (htm (:tr (:td (str (jsown:val queue "ref")))
                        (:td (str (jsown:val queue "predicate")))
                        (:td (str (if (equal tasks "[]") 0 (length tasks))))))))

        (:h2 "Waiting tasks:")
        (:ul
         (loop for queue in queues for tasks in qtasks do
              (htm (:li (str (jsown:val queue "ref"))
                        (unless (equal tasks "[]")
                          (htm (:ul
                                (loop for task in tasks do
                                     (htm (:li
                                           (:ul
                                            (:li :title (str (jsown:to-json task)) "id: " (str  (jsown:val task "ref")) )
                                            (:li "requirements:" (str (jsown:to-json(jsown:val task "requirements"))))
                                            (:li "state:" (str  (jsown:val task "state")))
                                            (:li "userContext:" (str (jsown:to-json(jsown:val task "userContext"))))))))))))))))))))

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 3000))

(defun start-server()
  (hunchentoot:start *server*))

(defun stop-server()
  (hunchentoot:stop *server*))
(rchecker::set-server :host "192.168.1.191")
