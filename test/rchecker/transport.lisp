(in-package #:rchecker)

(defun dump(cmd)
  (format t "~%~A"cmd) cmd)

(defun format-json(str)
  (with-input-from-string (f str)
    (UIOP:run-program "python -m json.tool" :ignore-error-status t :input f :output '(:string :stripped t)))
  str)

(defun exec-shell(cmd)
  (UIOP:run-program cmd :ignore-error-status t :input nil :output '(:string :stripped t)))

(defun cmd-curl(url method headers body)
  (format nil "curl -X ~A ~A ~{-H '~{~A~^:~}'~} ~@[-d '~A'~]"
          method url headers body))

(defun transport(url method headers &optional body)
  (let* ((str  (exec-shell (dump (cmd-curl url method headers body))))
         (json (jsown:parse str)))
    (dump (if (equal json t) str
              (format-json str)))
    (if (equal json t)str   json) ))

(defun api-endpoint(path transport)
  #'(lambda(url method headers &optional body)
      (funcall transport (format nil "~A~A" path url) method headers body)))

(defvar *transport* (api-endpoint "http://localhost:8080/comms-router-web/api" #'transport))
