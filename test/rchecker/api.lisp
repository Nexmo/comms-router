(in-package #:rchecker)
(defparameter *cells* (make-hash-table))
(defun clear-events() (setf *cells* (make-hash-table)))
(defun get-event(key) (assert (gethash key *cells*)) (gethash key *cells*))
(defun has-event(key) (gethash key *cells*))
(defun fire-event(key)  #'(lambda(val) (assert val) (setf (gethash key *cells*) val)))
(defun clear-event(key)  (setf (gethash key *cells*) nil))

(defun tr-request(method url &optional body)
  #'(lambda()
      (list (if (listp url)(format nil "~{~A~^/~}" url)
                url)
            method '(("Content-type" "application/json"))
            (when body  (jsown:to-json body))) ) )

(defun http-get (url &rest urls)  (tr-request :get (list* url urls)))
(defun http-del (url &rest urls)  (tr-request :delete (list* url urls)) )
(defun http-put(url body) (tr-request :put url body))
(defun http-post(url body) (tr-request :post url body))

(defun contains(key)
  #'(lambda(js)
      (member key (jsown:keywords js) :test #'equal)))

(defun get-id(&optional (prefix "tr")) (format nil "~A-~{~A~}~A~A" prefix (subseq (multiple-value-list (decode-universal-time (get-universal-time))) 0 4) (subseq (string (gensym))1) (random 1000)))

(defun tr-step(request check events)
  (let ((js (apply (funcall *endpoint* (transport-drakma)) (funcall request))))
    (when (funcall check js)
      (funcall events js))
    js))

(defun swagger()
  (tr-step (http-get "/swagger.json")
           (constantly t)
           #'identity))

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun time-to-str(scg-time)
  (if (numberp scg-time) (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
                             (decode-universal-time (unix-to-universal-time (floor (/ scg-time 1000))))
                           (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d/~d (GMT~@d)"
                                   hour
                                   minute
                                   second
                                   month
                                   date
                                   year
                                   (- tz)))
      scg-time))

(defun decode-date-kv(kv)
  (cond
    ((or (search "date" (first kv))
         (search "time" (first kv))
         (search "lastTimeAtBusyState" (first kv))) (list* (first kv) (time-to-str (rest kv))))
    (t (list* (first kv) (decode-dates (rest kv))))))

(defun decode-dates(l)
  (cond
    ((null l) l)
    ((not (listp l)) l)
    ((equal (first l) :obj) (list* :obj (mapcar #'decode-date-kv (rest l))))
    (t (list* (decode-dates (first l)) (decode-dates (rest l))))))

(defun router-all()
  (tr-step (http-get "/routers")
           #'(lambda(js)(and (listp js) (> (length js) 0) (funcall (contains "ref") (first js))))
           #'(lambda(js) (funcall (fire-event :router) (jsown:val (first js) "ref")))))

(defun router-new(&key (name "name") (description "description"))
  (tr-step (http-post "/routers" (jsown:new-js("name" name)
                                              ("description" description)))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :router) (jsown:val js "ref")))))

(defun router-put(&key (id (get-event :router)) (name "name") (description "description"))
  (tr-step (http-put (list "/routers" id) (jsown:new-js ("name" name)
                                                        ("description" description)))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :router) (jsown:val js "ref")))))
(defun router-set(&key (id (get-event :router)) (name "name") (description "description"))
  (tr-step (http-post(list "/routers" id) (jsown:new-js ("name" name)
                                                        ("description" description)))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :router) (jsown:val js "ref")))))

(defun router(&key (id (get-event :router)))
  (tr-step (http-get "/routers" id)
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :router) id))))

(defun router-del(&key (id (get-event :router)))
  (tr-step (http-del "/routers" id)
           #'(lambda(js) (equal js ""))
           #'(lambda(js) (clear-event :router))) )
;;; skill
(defun skill-all(&key (router-id (get-event :router)) (per-page 50) (page-number 1))
  (tr-step (http-get "/routers" router-id (format nil "skills?per_page=~A&page_num=~A" per-page page-number))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") (first js))))
           #'(lambda(js) (funcall (fire-event :skill) (jsown:val (first js) "ref")))))

(defun skill(&key (router-id (get-event :router))
               (id (get-event :skill)))
  (tr-step (http-get "/routers" router-id "skills" id)
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :skill) (jsown:val js "ref")))))

(defun skill-del(&key (router-id (get-event :router))
                   (id (get-event :skill)) )
  (tr-step (http-del  "/routers" router-id "skills" id)
           #'(lambda(js) (equal js ""))
           #'(lambda(js) (clear-event :skill))))

(defun skill-new(&key (router-id (get-event :router))
                   (description "description")
                   (multivalue :true)
                   (domain (jsown:new-js ("values" '("en" "es"))
                                         ("type" "enumeration"))))
  (tr-step (http-post (list "/routers" router-id "skills") (jsown:new-js
                                                             ("description" description)
                                                             ("domain" domain)
                                                             ("multivalue" multivalue)))
           #'(lambda(js)(and (listp js) (funcall (contains "ref") js)) )
           #'(lambda(js)(funcall (fire-event :skill) (jsown:val js "ref")))))

(defun skill-put(&key (router-id (get-event :router))
                   (id (get-event :skill))
                   (address "address")
                   (capabilities (jsown:new-js ("language" "en"))))
  (tr-step (http-put (list "/routers" router-id "skills" id) (jsown:new-js
                                                               ("address" address)
                                                               ("capabilities" capabilities) ))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :skill) (jsown:val js "ref")))))
(defun skill-set(&key (router-id (get-event :router))
                   (id (get-event :skill))
                   (address "address")
                   (state "ready") ;; offline busy
                   (capabilities (jsown:new-js ("language" "en"))))
  (tr-step (http-post (list "/routers" router-id "skills" id) (jsown:new-js
                                                                ("address" address)
                                                                ("state" state)
                                                                ("capabilities" capabilities)))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :skill) (jsown:val js "ref")))))

;;; agent
(defun agent-all(&key (router-id (get-event :router)) (per-page 50) (page-number 1))
  (tr-step (http-get "/routers" router-id (format nil "agents?per_page=~A&page_num=~A" per-page page-number))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") (first js))))
           #'(lambda(js) (funcall (fire-event :agent) (jsown:val (first js) "ref")))))

(defun agent(&key (router-id (get-event :router))
               (id (get-event :agent)))
  (tr-step (http-get "/routers" router-id "agents" id)
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :agent) (jsown:val js "ref")))))



(defun agent-del(&key (router-id (get-event :router))
                   (id (get-event :agent)) )
  (tr-step (http-del  "/routers" router-id "agents" id)
           #'(lambda(js) (equal js ""))
           #'(lambda(js) (clear-event :agent))))

(defun agent-new(&key (router-id (get-event :router))
                   (address "address")
                   (tag "some-info")
                   (capabilities (jsown:new-js ("language" "en")
                                               ("tag" tag))))
  (tr-step (http-post (list "/routers" router-id "agents") (jsown:new-js
                                                             ("address" address)
                                                             ("capabilities" capabilities)))
           #'(lambda(js)(and (listp js) (funcall (contains "ref") js)) )
           #'(lambda(js)(funcall (fire-event :agent) (jsown:val js "ref")))))

(defun agent-put(&key (router-id (get-event :router))
                   (id (get-event :agent))
                   (address "address")
                   (capabilities (jsown:new-js ("language" "en"))))
  (tr-step (http-put (list "/routers" router-id "agents" id) (jsown:new-js
                                                               ("address" address)
                                                               ("capabilities" capabilities) ))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :agent) (jsown:val js "ref")))))
(defun agent-set(&key (router-id (get-event :router))
                   (id (get-event :agent))
                   (address "address")
                   (state "ready") ;; offline busy
                   (capabilities (jsown:new-js ("language" "en"))))
  (tr-step (http-post (list "/routers" router-id "agents" id) (jsown:new-js
                                                               ("address" address)
                                                               ("state" state)
                                                               ("capabilities" capabilities)))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :agent) (jsown:val js "ref")))))

(defun insert-into(table values)
  (let ((cmd (format nil "mysql --database=comms_router_core -u root --password=comms-router -N -e 'insert into ~A VALUES (~{~A~^,~})'"
                     table values)))
    (format t "~%~A" cmd)
    (UIOP:run-program cmd :input nil :output '(:string :stripped t) :IGNORE-ERROR-STATUS t)
    ))
(defun add-quote(text)(format nil "\"~A\"" text))
(defun bind-agent-to-queue(&key (agent-id (get-event :agent))
                     (queue-id (get-event :queue)))
  (insert-into "agent_queue" (list (add-quote agent-id) (add-quote queue-id))))
;;; plan
(defun plan-all(&key (router-id (get-event :router)) )
  (tr-step (http-get "/routers" router-id "plans")
           #'(lambda(js) (and (listp js) (funcall (contains "ref") (first js))))
           #'(lambda(js) (funcall (fire-event :plan) (jsown:val (first js) "ref")))))

(defun plan-new(&key (router-id (get-event :router))
                  (queue-id (get-event :queue))
                  (default-queue-id (get-event :queue))
                  (predicate "1 ==1")
                  (priority 0)
                  (next-route nil)
                  (timeout 3600)
                  (rules (list (jsown:new-js ("tag" "test-rule")
                                             ("predicate" predicate)
                                             ("routes" (append
                                                        (list (jsown:new-js
                                                                ("queueRef" queue-id)
                                                                ("priority" priority)
                                                                ("timeout" timeout)))
                                                        next-route)))))
                  (default-route (jsown:new-js
                                   ("queueRef" default-queue-id)
                                   ("priority" 0)
                                   ("timeout" 360000)))
                  (description "description"))
  (tr-step (http-post (list "/routers" router-id "plans")
                      (jsown:new-js
                        ("rules" rules)
                        ("description" description)
                        ("defaultRoute" default-route)))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :plan) (jsown:val js "ref")))))

(defun plan-set(&key (router-id (get-event :router))
                  (id (get-event :plan))
                  (tag (get-id "test-rule"))
                  (predicate "1 == 1")
                  (description "plan description")
                  (queue (get-event :queue))
                  (rules (list (jsown:new-js ("tag" tag) ("predicate" predicate) ("queueRef" queue)))) )
  (tr-step (http-post (list "/routers" router-id "plans" id)
                      (jsown:new-js
                                   ("rules" rules)
                                   ("description" description)                                    ))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :queue) (jsown:val js "ref")))))
(defun plan(&key (router-id (get-event :router))
              (id (get-event :plan)))
  (tr-step (http-get "/routers" router-id "plans" id )
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :plan) (jsown:val js "ref")))))

(defun plan-put(&key (router-id (get-event :router))
                  (id (get-event :plan))
                  (tag (get-id "test-rule"))
                  (predicate "1 == 1")
                  (description "plan description")
                  (queue (get-event :queue))
                  (rules (list (jsown:new-js ("tag" tag) ("predicate" predicate) ("queueRef" queue)))) )
  (tr-step (http-put (list "/routers" router-id "plans" id)
                     (jsown:new-js ("rules" rules)
                                   ("description" description)                                    ))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :plan) (jsown:val js "ref")))))

(defun plan-del(&key (router-id (get-event :router))
                  (id (get-event :plan)))
  (tr-step (http-del "/routers" router-id "plans" id)
           #'(lambda(js) (equal js ""))
           #'(lambda(js) (clear-event :plan))))

;;; queue
(defun queue-all(&key (router-id (get-event :router)) (per-page 50) (page-number 1) )
  (tr-step (http-get "/routers" router-id (format nil "queues?per_page=~A&page_num=~A" per-page page-number))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") (first js))))
           #'(lambda(js) (funcall (fire-event :queue) (jsown:val (first js) "ref")))))

(defun queue(&key (router-id (get-event :router))
               (id (get-event :queue)))
  (tr-step (http-get "/routers" router-id "queues" id)
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :queue) (jsown:val js "ref")))))

(defun queue-size(&key (router-id (get-event :router))
               (id (get-event :queue)))
  (tr-step (http-get "/routers" router-id "queues" id "size")
           #'(lambda(js)(and (listp js) (funcall (contains "size") js)))
           #'(lambda(js) (funcall (fire-event :queue) id))))

(defun queue-tasks(&key (router-id (get-event :router))
                    (id (get-event :queue)))
  (tr-step (http-get "/routers" router-id "queues" id "tasks")
           #'(lambda(js)(and (listp js) (funcall (contains "size") js)))
           #'(lambda(js) (funcall (fire-event :queue) id))))

(defun queue-put(&key (router-id (get-event :router))
                   (id (get-event :queue))
                   (description "description")
                   (predicate "1==1") )
  (tr-step (http-put (list "/routers" router-id "queues" id) (jsown:new-js
                                                               ("description" description)
                                                               ("predicate" predicate)))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :queue) (jsown:val js "ref")))))

(defun queue-set(&key (router-id (get-event :router))
                   (id (get-event :queue))
                   (description "description")
                   (predicate :null) )
  (tr-step (http-post (list "/routers" router-id "queues" id)
                      (jsown:new-js
                       ("description" description)
                       ("predicate" predicate)))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :queue) (jsown:val js "ref")))))

(defun queue-del(&key (router-id (get-event :router))
                   (id (get-event :queue)))
  (tr-step (http-del "/routers" router-id "queues" id)
           #'(lambda(js) (equal js ""))
           #'(lambda(js) (clear-event :queue))))

(defun queue-new(&key (router-id (get-event :router))
                   (description "description")
                   (predicate "1==1"))
  (tr-step (http-post (list "/routers" router-id "queues")
                      (jsown:new-js ("description" description)
                                    ("predicate" predicate)))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :queue) (jsown:val js "ref")))))
;;; task
(defun task-all(&key (router-id (get-event :router))
                  (per-page 50) (page-number 1))
  (tr-step (http-get "/routers" router-id (format nil "tasks?per_page=~A&per_num" per-page page-number))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") (first js))))
           #'(lambda(js) (funcall (fire-event :task) (jsown:val (first js) "ref")))))

(defun task(&key (router-id (get-event :router))
              (id (get-event :task)))
  (tr-step (http-get "/routers" router-id "tasks" id)
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :task) (jsown:val js "ref")))))

(defun task-context(&key (router-id (get-event :router))
              (id (get-event :task)))
  (tr-step (http-get "/routers" router-id "tasks" id "user_context")
           #'(lambda(js) (and (listp js) ))
           #'(lambda(js) )))

(defun task-put(&key (router-id (get-event :router))
                  (id (get-event :task))
                  (requirements (jsown:new-js ("key" t)))
                  (callback-url (format nil "http://192.168.1.171:8787/?task"))
                  (queue-id (get-event :queue))
                  (plan-id :null))
  (tr-step (http-put (list "/routers" router-id "tasks" id)
                     (jsown:new-js
                       ("callbackUrl" callback-url)

                       ("requirements" requirements)
                       ("queueRef" queue-id)
                       ("planRef" plan-id)))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :task) (jsown:val js "ref")))))

(defun task-set(&key (router-id (get-event :router))
                  (id (get-event :task))
                  (state "completed")
                  )
  (tr-step (http-post (list "/routers" router-id "tasks" id)
                     (jsown:new-js ("state" state)
                                   ))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :task) (jsown:val js "ref")))))
(defun task-del(&key (router-id (get-event :router))
                   (id (get-event :task)))
  (tr-step (http-del  "/routers" router-id "tasks" id)
           #'(lambda(js)(equal js ""))
           #'(lambda(js) (clear-event :task))))

(defun task-new(&key (router-id (get-event :router))

                  (requirements (jsown:new-js ("key" t)))
                  (callback-url (format nil "http://localhost:4343/task?router=~A&sleep=~A" router-id (random 2)))
                  (context (jsown:new-js ("key" "value")))
                  (queue-id (get-event :queue))
                  (plan-id :null) )
  (tr-step (http-post (list "/routers" router-id "tasks")
                      (jsown:new-js
                        ("callbackUrl" callback-url)
                        ("userContext" context)
                        ("requirements" requirements)
                        ("queueRef" queue-id)
                        ("planRef" plan-id)
                        ))
           #'(lambda(js) (and (listp js) (funcall (contains "ref") js)))
           #'(lambda(js) (funcall (fire-event :task) (jsown:val js "ref")))))

(defun ptask-new(&key (router-id (get-event :router))
                   (requirements (jsown:new-js))
                   (callback-url (format nil "http://192.168.1.171:8787/?task="))
                   (context (jsown:new-js ("key" "value")))
                   (plan-id (get-event :plan)))
  (task-new :router-id router-id
            :requirements requirements
            :callback-url callback-url
            :queue-id :null
            :context context
            :plan-id plan-id))
