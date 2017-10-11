(in-package :rchecker)

(defun thunk-result(request response)
  #'(lambda()(list request response)))

(defun check-result(result description)
  #'(lambda(res)(list result description)))

(defun tstep-result(description response check steps-to-reproduce check-description)
  #'(lambda()
      (list response check (list (list description check steps-to-reproduce response check-description)))))

(defun tstep(description thunk-fn check-fn)
  #'(lambda()
      (destructuring-bind (steps-to-reproduce result)(funcall thunk-fn)
        (destructuring-bind ( check-result check-description)(funcall check-fn result)
          (list result check-result (list (list description check-result steps-to-reproduce (jsown:to-json result) check-description)))))))

(defun step-bind(step p-step)
  #'(lambda()
      (destructuring-bind (result check description)(funcall step)
        (if check
            (destructuring-bind (result1 check1 description1) (funcall(funcall p-step result))
              (list result1 check1 (append description description1)))
            (list result check description))) ))

(defun tand(step &rest steps)
  (if steps
      (step-bind step
        #'(lambda(res)
            (apply #'tand steps)))
      step))

(defun twait (step &key (timeout 10) (delay 1) (end-time (+ (* timeout internal-time-units-per-second)
                                                  (get-internal-real-time))))
  #'(lambda()
      (destructuring-bind (response result description) (funcall step)
        (if result
            (list response  result description)
            (if (> end-time (get-internal-real-time))
                (progn (sleep delay) (funcall (twait step :end-time end-time :delay delay)))
                (list response  result (append description (list(list "Waiting failed" nil (format nil "wait for ~A sec"timeout) (jsown:to-json response) (list "Waiting step to complete has failed"))))))) ) ))

(defun tapply(request)
  #'(lambda()
      (list (apply #'cmd-curl (funcall request))
            (apply *transport* (funcall request)))))



(defmacro tlet(vars &body body)
  `(step-bind ,(first (last (first vars)) )
              ,(if (butlast(subseq (first vars) 1))
                  `(compose #'(lambda(,(first (first vars)))
                                 ,(if (rest vars)
                                      `(tlet ,(rest vars) ,@body)
                                      `(progn ,@body) ) )
                            ,@(butlast(subseq (first vars) 1)))
                  `#'(lambda(,(first (first vars)))
                      ,(if (rest vars)
                           `(tlet ,(rest vars) ,@body)
                           `(progn ,@body) ) ) ) ))

(defun print-log(result)
  (destructuring-bind (result check log)result
    (format t "~{ ~{~%Description:~A~%Result:~:[FAIL~;pass~]~%Request:~A~%Response:~A~%~{Checks:~A~%~}~}~}"log)
    (format t "Result:~:[FAIL~;pass~]" check)))
