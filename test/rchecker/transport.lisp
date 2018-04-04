(in-package #:rchecker)

(defun dump(cmd) (format t "~%~A"cmd) cmd)

(defun format-json(str)
  (with-input-from-string (f str)
    (UIOP:run-program "python -m json.tool" :ignore-error-status t :input f :output '(:string :stripped t)))
  str)

(defun exec-shell (cmd &optional (input nil))
  (multiple-value-bind (res err code)(UIOP:run-program cmd :ignore-error-status t :input input :output '(:string :stripped t) :error-output '(:string :stripped t))
    (format nil "~A~A"res err)))

(defun cmd-curl()
  #'(lambda(url method headers body)
      (format nil "curl -s -X ~A '~A' ~{-H '~{~A~^:~}'~} ~@[-d ~S~]"
              method url headers body)))

(defun drakma-client()
  #'(lambda(url method headers body)
      (let ((res (drakma:http-request
                  url :method method
                  ;;:accept "application/json"
                  :content-type (first(rest(assoc "Content-type" headers :test #'equal)))
                  :additional-headers (mapcar #'(lambda(h)(cons (first h) (second h)))
                                              (remove-if #'(lambda(h)(member (first h)
                                                                             '("Content-type")
                                                                             :test #'equal))
                                                         headers) )
                  :content body)))
        (if (stringp res) res
            (babel:octets-to-string res ) )) ))

(defun dexador-client()
  #'(lambda(url method headers body)
      (let ((cons-headers (mapcar #'cons (mapcar #'first headers) (mapcar #'second headers))))
        (handler-case
            (handler-bind ((dex:http-request-failed (dex:retry-request 5 :interval 3)))
              (dex:request url :method method :headers cons-headers
                           :content body))
            
          (dex:http-request-bad-request (e)

            (dex:response-body e)
            ;; Runs when 400 bad request returned
            )
          (dex:http-request-failed (e) ;; For other 4xx or 5xx
            (format *error-output* "Dexador error: The server returned ~D" (dex:response-body e))
            (dex:response-body e))) ) ))

(defun content-json(fn)
  #'(lambda (url method headers body)
      (let*((str (funcall fn url method headers body) )
            (json (if (zerop(length str) )  ""
                      (unless (equal "" str) 
                        (handler-case (jsown:parse str)
                          (error (e) (progn (format t "~S"e) t))))) ))
        ;(dump (if (member json '(t nil) ) str (format-json str)))
        (if (member json '(t nil)) str json))))

(defun transport()
  (content-json (compose #'exec-shell #'dump (cmd-curl))))

(defun transport-drakma()
  (content-json (drakma-client)))

(defun api-endpoint(path)
  #'(lambda(transport)
      #'(lambda(url method headers &optional body)
          (funcall transport (format nil "~A~A" path url) method headers body))) )

(defun set-endpoint(&key(protocol "http") (prefix "comms-router-web/api") (host "localhost")(port 8080))
  (api-endpoint (format nil "~A://~A:~A/~A" protocol host port prefix)))

(defvar *endpoint* (set-endpoint))

(defun set-server (&key(protocol "http") (host "localhost")(port 8080) (prefix "comms-router-web/api"))
  (setf *endpoint* (set-endpoint :protocol protocol :host host :port port :prefix prefix)))
(defun set-server-dev()
  (set-server :host "192.168.1.166" :prefix "comms-router-web-pure/api")
)
