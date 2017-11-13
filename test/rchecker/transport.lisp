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

(defun content-json(fn)
  #'(lambda (url method headers body)
      (let*((str (funcall fn url method headers body))
            (json (unless (equal "" str)
                    (handler-case (jsown:parse str)
                      (error (e) (progn (format t "~S"e) t))))))
        (dump (if (member json '(t nil) )
                  str
                  (format-json str)))
        (if (member json '(t nil)) str json))))

(defun transport()
  (content-json (compose #'exec-shell #'dump (cmd-curl))))

(defun transport-drakma()
  (content-json (drakma-client)))

(defun api-endpoint(path)
  #'(lambda(transport)
      #'(lambda(url method headers &optional body)
          (funcall transport (format nil "~A~A" path url) method headers body))) )

(defun set-endpoint(&key(protocol "http") (host "localhost")(port 8080))
  (api-endpoint (format nil "~A://~A:~A/comms-router-web/api" protocol host port)))

(defvar *endpoint* (set-endpoint))

(defun set-server (&key(protocol "http") (host "localhost")(port 8080))
  (setf *endpoint* (set-endpoint :protocol protocol :host host :port port)))
