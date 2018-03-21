(in-package :rchecker)
(defparameter *times* ())
(defparameter *queue* (queues:make-queue :simple-cqueue))
(defun autocomplete-task(router-id task-id)
  (destructuring-bind (response check description)
		      (funcall (tand
				(etask :router-id router-id :id task-id :state "assigned")
				(etask-set :router-id router-id :id task-id :state "completed")))
		      (list (list response check description)
			    (funcall (tand (etask-set-context :router-id router-id :task-id task-id :key "result" :value (if check t :false))
					   (let ((descr (format nil "~S" description)))
					     (etask-set-context  :router-id router-id :task-id task-id
								 :key "log"
								 :value (ppcre:regex-replace-all "'" ""
												 (subseq descr (max 0 (- (length descr) 255)))))) ) )) ) )

(defun fill-queue (&key (host "localhost:4343") (router (get-event :router)) (queue (get-event :queue)) (max-tasks 10) (delay 4))
  (let* ((res (funcall (equeue-size :router-id router :id queue)))
         (run-info (with-output-to-string(s)
                     (let ((*standard-output* s))
                       (if (second res)
                           (let ((observed-tasks (jsown:val (first res) "size"))
                                 (expected-tasks max-tasks))
                             (if (> expected-tasks observed-tasks )
				 (loop for res = (funcall (etask-new
						  :router-id router :queue-id queue
						  :callback-url (format nil "http://~A/schedule-task?delay=~A&max-tasks=~A&host=~A" host delay max-tasks (do-urlencode:urlencode host))))  :while (< (jsown:val (first res) "queueTasks") expected-tasks))
                                 "queue is full"))
                           (print-log res)) ) )))
    run-info ))

(defvar *lock* (bt:make-lock))
(defvar *to-complete* ())

(hunchentoot:define-easy-handler (app :uri "/task") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((body (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data)))))
    ;;(hunchentoot:log-message* 'task-requ "~(~A~):~A Body:~A" "app" (hunchentoot:query-string*) body)
    (let* ((start-time (get-internal-real-time))
           (task-info (jsown:parse body))
           (task (jsown:val task-info "task"))
           (task-id (jsown:val task "ref"))
           (router-id (jsown:val task "routerRef"))
           (delay (hunchentoot:parameter "sleep")))
      (bt:make-thread #'(lambda()
                          (sleep (parse-integer delay))
                          (let ((res (autocomplete-task router-id task-id)))
                            (if (some #'null (mapcar #'second res))
                                "ERROR"
                                (push (get-internal-real-time) *times*))))
                      :name "auto-completer")
      ;;(hunchentoot:log-message* 'ready "completed in ~A" (- (get-internal-real-time) start-time))
      "OK")))

(hunchentoot:define-easy-handler (push-task :uri "/push-task") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((body (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data)))))
    ;;(hunchentoot:log-message* 'task-requ "~(~A~):~A Body:~A" "app" (hunchentoot:query-string*) body)
    (let* ((start-time (get-internal-real-time))
           (task-info (jsown:parse body))
           (task (jsown:val task-info "task"))
           (task-id (jsown:val task "ref"))
           (router-id (jsown:val task "routerRef"))
           (delay (hunchentoot:parameter "sleep")))
      (queues:qpush *queue* (list router-id task-id delay (get-internal-real-time)))
      ;;(hunchentoot:log-message* 'ready "completed in ~A" (- (get-internal-real-time) start-time))
      "OK")))

(hunchentoot:define-easy-handler (handle-slow :uri "/handle-slow") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((body (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data)))))
    (hunchentoot:log-message* 'slow-requ "~(~A~):~A Body:~A" "app" (hunchentoot:query-string*) body)
    (let* ((start-time (get-internal-real-time))
           (task-info (jsown:parse body))
           (task (jsown:val task-info "task"))
           (task-id (jsown:val task "ref"))
           (router-id (jsown:val task "routerRef"))
           (delay (hunchentoot:parameter "sleep"))
           (delay-after (hunchentoot:parameter "sleep-after")))
      (sleep (parse-integer delay))
      (let ((res (autocomplete-task router-id task-id)))
        (if (some #'null (mapcar #'second res))
            "ERROR"
            (push (get-internal-real-time) *times*)))
      (sleep (parse-integer delay-after))
      (hunchentoot:log-message* 'ready "completed in ~A" (- (get-internal-real-time) start-time))
      "OK")))

(hunchentoot:define-easy-handler (recreate-task :uri "/complete-and-create-task") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((body (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data)))))
    ;;(hunchentoot:log-message* 'requ "~(~A~):~A Body:~A" "app" (hunchentoot:query-string*) body)
    (let* ((start-time (get-internal-real-time))
           (task-info (jsown:parse body))
           (task (jsown:val task-info "task"))
           (task-id (jsown:val task "ref"))
           (router-id (jsown:val task "routerRef"))
           (delay (hunchentoot:parameter "sleep")))
      (bt:make-thread #'(lambda()
                          (sleep (parse-integer delay))
                          (let ((res (funcall (tand (etask-set :router-id router-id :id task-id :state "completed")
                                                    (etask-new :router-id router-id :callback-url (jsown:val task "callbackUrl"))
                                                    ))))
                            (push (if (second res) (get-internal-real-time) res)
                                  *times*)) )
                      :name "auto-completer")
      (hunchentoot:log-message* 'ready "completed in ~A" (- (get-internal-real-time) start-time))
      "OK")))
(defparameter *enabled* t)

(hunchentoot:define-easy-handler (enable :uri "/enable") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (setf *enabled* t)
  "enabled")

(hunchentoot:define-easy-handler (disable :uri "/disable") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (setf *enabled* nil)
  "disabled")

(hunchentoot:define-easy-handler (create-tasks :uri "/create-tasks") (router queue max-tasks delay host)
  (setf (hunchentoot:content-type*) "text/plain")
  (when *enabled*
    (let ((queue-id queue)
          (router router)
          (max-tasks max-tasks)
          (host host)
          (delay delay))
      (bt:make-thread
       #'(lambda()
           (fill-queue :host (if host (do-urlencode:urldecode host) "localhost:4343") :router router :queue queue-id :max-tasks (parse-integer max-tasks) :delay delay)
           :name "auto-completer"))))
  "OK")

(hunchentoot:define-easy-handler (schedule-task :uri "/schedule-task") (delay max-tasks host)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((body (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data)))))
    (hunchentoot:log-message* 'schedule "~(~A~):~A Body:~A" "app" (hunchentoot:query-string*) body)
    (let* ((start-time (get-internal-real-time))
           (task-info (jsown:parse body))
           (task (jsown:val task-info "task"))
           (queue-id (jsown:val task "queueRef"))
           (task-id (jsown:val task "ref"))
           (router-id (jsown:val task "routerRef")))
      (bt:make-thread
       #'(lambda()
           (drakma:http-request
            (format nil "http://localhost:4343/create-tasks?max-tasks=~A&delay=~A&router=~A&queue=~A&host=~A"
                    max-tasks delay router-id queue-id (if host (do-urlencode:urlencode host) host))))
       :name "fill-queue" )
      (bt:make-thread
       #'(lambda()
           (sleep (parse-integer delay))
           (drakma:http-request (format nil "http://localhost:4343/complete-task?max-tasks=~A&delay=~A&router=~A&task=~A&queue=~A" max-tasks delay router-id task-id queue-id))
           :name "auto-completer")
       :name "wait-to-complete")
      "OK")))

(hunchentoot:define-easy-handler (drakma-queue-size :uri "/queue-size") (max-tasks delay router task queue)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~{~A~^,~}"(mapcar (compose (js-val "size") #'(lambda(id) (queue-size :id id)) (js-val "ref")) (queue-all)))  )
(hunchentoot:define-easy-handler (drakma-agent-state :uri "/agent-state") (max-tasks delay router task queue)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~{~A~^,~}"(mapcar (compose (js-val "state") #'(lambda(id) (agent :id id)) (js-val "ref")) (agent-all)))  )

(hunchentoot:define-easy-handler (complete-task :uri "/complete-task") (max-tasks delay router task queue)
  (setf (hunchentoot:content-type*) "text/plain")
  (bt:make-thread
   #'(lambda()
       (funcall (etask-set :router-id router :id task :state "completed"))
       (push (get-internal-real-time) *times*) )
   :name "complete task")
  "OK")



(hunchentoot:define-easy-handler (task-completed :uri "/throughput") ()
  (setf (hunchentoot:content-type*) "text/html")
  (let((template "<html>
  <head>
    <script type=\"text/javascript\" src=\"https://www.gstatic.com/charts/loader.js\"></script>
    <script type=\"text/javascript\">
      google.charts.load('current', {'packages':['corechart']});
      google.charts.setOnLoadCallback(drawChart);

      function drawChart() {
        var data = google.visualization.arrayToDataTable([
          ['minute', 'count'],
          ~{~{['~A',~A]~}~^,~}
        ]);

        var options = {
          title: 'Performance',
          curveType: 'function',
          legend: { position: 'bottom' }
        };

        var chart = new google.visualization.LineChart(document.getElementById('curve_chart'));

        chart.draw(data, options);
      }
    </script>
  </head>
  <body>
    <div id='curve_chart' style='width: 900px; height: 500px'></div>
    </body>
    </html>"))
      (format nil template (let((current (first *times*)))
                             (mapcar #'(lambda(offset)
                                         (list offset (floor (/ (count-if #'(lambda(timestamp)
                                                                              (let ((timediff (- (- current (* offset (* 60 internal-time-units-per-second)))  
                                                                                                 timestamp)))
                                                                                (and (not (listp timestamp))
                                                                                     (< timediff (* 120 internal-time-units-per-second))
                                                                                     (>= timediff 0)                                                   )))
                                                                          *times*) 2))))
                                     (loop :for x from 0 to 50 :collect x))
                             ))
      ))

(hunchentoot:define-easy-handler (retry :uri "/503") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (setf (hunchentoot:return-code*) (list 503))
  (hunchentoot:log-message* 'requ "~(~A~):~A Body:~A" "app" (hunchentoot:query-string*) (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data))))
  "reserved for testing")

(hunchentoot:define-easy-handler (not-found :uri "/404") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (setf (hunchentoot:return-code*) (list 503))
  "reserved for testing")

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 4343
                                ;;:taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)
                                ))
(defun start-server()  (hunchentoot:start *server*))
(defun stop-server()  (hunchentoot:stop *server*))

;;(setf (hunchentoot:TASKMASTER-MAX-THREAD-COUNT (hunchentoot::acceptor-taskmaster *server*)) 400)
;;(setf (hunchentoot::acceptor-access-log-destination *server*) nil)
;;(drakma:http-request (format nil "http://localhost:4343/create-tasks?router=~A&queue=~A&max-tasks=10" (get-event :router) (get-event :queue)) )
;;(drakma:http-request (format nil "http://localhost:4343/schedule-task?router=~A&queue=~A&task=~A" (get-event :router) (get-event :queue) (get-event :task)) )
(defparameter *queue-tag* 0)
(defun add-queue-agents (&key(queues 10) 
                         (max-tasks 30) 
                         (task-delay 120)
                         (insert-delay 30)
                         (agent-per-queue 5)
                         (host "localhost:4343"))
  (loop for x from 1 to queues do
       (let ((queue (jsown:val (queue-new) "ref")))
         (queue-set :predicate (format nil "#{queue}=='~A'" queue))
	 (drakma:http-request
	  (format nil "http://localhost:4343/create-tasks?max-tasks=~A&delay=~A&router=~A&queue=~A&host=~A"
                    max-tasks task-delay (get-event :router) (get-event :queue) (if host (do-urlencode:urlencode host) "localhost:4343")) )
         (loop :repeat agent-per-queue do (agent-new ) (agent-set :capabilities (jsown:new-js ("queue" queue))))
         (sleep insert-delay) ) ))
;;
