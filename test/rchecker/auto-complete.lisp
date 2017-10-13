(in-package :rchecker)
(defun autocomplete-task(router-id task-id)
  (destructuring-bind (response check description)
      (funcall (tand
                (etask :router-id router-id :task-id task-id :state "assigned")
                (etask-set :router-id router-id :task-id task-id :state "completed")))
    (append description
            (funcall (tand (etask-set-context :router-id router-id :task-id task-id :key "result" :value (if check t :false))
                           (let ((descr (format nil "~S" description)))
                             (etask-set-context  :router-id router-id :task-id task-id  :key "log" :value (ppcre:regex-replace-all "'" "" (subseq descr (max 0 (- (length descr) 255))))))) )) ) )

(hunchentoot:define-easy-handler (app :uri "/task") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((body (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data)))))
    ;;(hunchentoot:log-message* 'requ "~(~A~):~A Body:~A" "app" (hunchentoot:query-string*) body)

    (let ((task-info (jsown:parse body))
          (router-id (hunchentoot:parameter "router"))
          (delay (hunchentoot:parameter "sleep")))
      (sleep (parse-integer delay))
      (let ((res (autocomplete-task router-id (jsown:val (jsown:val task-info "task") "id"))))
        (when (some #'null (mapcar #'second res))
          (hunchentoot:log-message* 'ERROR! "~A" (autocomplete-task router-id (jsown:val (jsown:val task-info "task") "id"))) )
        "OK" )))

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 4343))
(defun start-server()  (hunchentoot:start *server*))
