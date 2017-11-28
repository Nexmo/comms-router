(in-package :rchecker)
(defparameter *times* ())
(defun autocomplete-task(router-id task-id)
  (destructuring-bind (response check description)
      (funcall (tand
                (etask :router-id router-id :id task-id :state "assigned")
                (etask-set :router-id router-id :id task-id :state "completed")))
    (list (list response check description)
            (funcall (tand (etask-set-context :router-id router-id :task-id task-id :key "result" :value (if check t :false))
                           (let ((descr (format nil "~S" description)))
                             (etask-set-context  :router-id router-id :task-id task-id
                                                 :key "log" :value (ppcre:regex-replace-all "'" ""
                                                                                            (subseq descr (max 0 (- (length descr) 255))))))
                           ) )) ) )

(defvar *lock* (bt:make-lock))
(defvar *to-complete* ())

(hunchentoot:define-easy-handler (app :uri "/task") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((body (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data)))))
    (hunchentoot:log-message* 'requ "~(~A~):~A Body:~A" "app" (hunchentoot:query-string*) body)
    (let* ((task-info (jsown:parse body))
           (task (jsown:val task-info "task"))
           (task-id (jsown:val task "id"))
           (router-id (jsown:val task "routerId"))
           (delay (hunchentoot:parameter "sleep")))
      (bt:make-thread #'(lambda()
                          (sleep (parse-integer delay))
                          (let ((res (autocomplete-task router-id task-id)))
                            (if (some #'null (mapcar #'second res))
                                "ERROR"
                                (push (get-internal-real-time) *times*)))
                          )
                      :name "auto-completer")

      "OK")))

(hunchentoot:define-easy-handler (retry :uri "/503") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (setf (hunchentoot:return-code*) (list 503))
  (hunchentoot:log-message* 'requ "~(~A~):~A Body:~A" "app" (hunchentoot:query-string*) (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data))))
  "reserved for testing")

(hunchentoot:define-easy-handler (not-found :uri "/404") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (setf (hunchentoot:return-code*) (list 503))
  "reserved for testing")

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 4343))
(defun start-server()  (hunchentoot:start *server*))
(defun stop-server()  (hunchentoot:stop *server*))
