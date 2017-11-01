(in-package :rchecker)

(defun thunk-result(request response)
  #'(lambda()(list request response)))

(defun check-result(result description)
  #'(lambda(res)(list result description)))

(defun tstep-result(description response check steps-to-reproduce check-description &optional sub-items)
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
(assert (equal (funcall(tand (tstep-result 'description 'response 'check 'steps-to-repro '(check-descr))
                       (tstep-result 'description1 'response1 'check1 'steps-to-repro1 '(check-descr1))))
               '(RESPONSE1 CHECK1
                 ((DESCRIPTION CHECK STEPS-TO-REPRO RESPONSE (CHECK-DESCR))
                  (DESCRIPTION1 CHECK1 STEPS-TO-REPRO1 RESPONSE1 (CHECK-DESCR1)))) ) )

(defun twait (step &key (timeout 10) (delay 1) (end-time (+ (* timeout internal-time-units-per-second)
                                                  (get-internal-real-time))))
  #'(lambda()
      (destructuring-bind (response result description) (funcall step)
        (if result
            (list response  result description)
            (if (> end-time (get-internal-real-time))
                (progn (sleep delay) (funcall (twait step :end-time end-time :delay delay :timeout timeout)))
                (list response  result
                      (append description (list(list "Waiting failed" nil
                                                     (format nil "wait for ~A sec" timeout)
                                                     (jsown:to-json response)
                                                     (list "Waiting step to complete has failed"))))))))))

(defun suite-result(results)
  (reduce #'(lambda(state res)(destructuring-bind (pass skip fail) state (if (second res) (list (1+ pass) skip fail)
                                                                             (list pass  skip (1+ fail)))))
          results :initial-value (list 0 0 0)))

(defun tsuite(description &rest steps)
  #'(lambda()
      (let* ((results (mapcar #'funcall steps))
             (result (every #'second results)))
        (funcall (tstep-result description
                               (suite-result results)
                               result
                               (format nil "Executing tests: ~{~S~^, ~}." (mapcar #'first (mapcar #'first (mapcar #'third results))))
                               (if result (list "ok - All tests have passed.")
                                   (mapcar #'(lambda(case-info) (format nil "FAIL - case ~S has failed."(first(first case-info)) ))
                                           (mapcar #'third results)))
                               results)))))

(assert (funcall (tsuite "Sample suite"
                         (tstep-result 'description 'response 'check 'steps-to-repro '(check-descr))
                         (tstep-result 'description1 'response1 'check1 'steps-to-repro1 '(check-descr1))
                         )))

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

(defun print-log(result &optional indent)
  (destructuring-bind (result check log)result
    (format t "~{~%---------------~{~%Description:~A~%Result:~:[FAIL~;pass~]~%Request:~A~%Response:~A~%~{Checks:~A~%~}~}~}" log)

    (format t "Result:~:[FAIL~;pass~]" check))
  (second result))

(defun remove-char(pos string)
  (concatenate 'string (subseq string 0 pos) (subseq string (1+ pos))))

(defun shrink-string (string fn)
  (loop for x from 1 to (length string)
     for small = (remove-char (1- x) string)
     :if (funcall fn small) :collect small) )

(defun shrink-some (string fn n)
  (loop for x from 1 to n
     for small = (remove-char (random n) string)
     :if (funcall fn small) :collect small) )

(defun find-smallest(candidates fn)
  (when candidates
    (let((smallest (shrink-string (first candidates) fn )))
      (format t "~%~A->~A" (length (first candidates)) smallest)
      (if smallest
          (find-smallest (append smallest (rest candidates)) fn)
          (if (rest candidates)
              (find-smallest (rest candidates) fn)
              (remove-if-not  fn candidates) ) )) ))

(defun fand(check &rest checks)
  (if checks
      #'(lambda(js)
          (when (funcall check js)
            (funcall (apply #'fand checks) js)))
      check))

(defun fnot(check)
  #'(lambda(js)
      (not (funcall check js))))

(defun for(check &rest checks)
  (if checks
      #'(lambda(js)
          (if (funcall check js) t
              (funcall (apply #'for checks) js)))
      check))

(defun fhas-json() #'(lambda(js)(and (listp json) (not (null json)))))
(defun fhas-key(key) #'(lambda(js)(jsown:keyp js key)))
(defun fhas-kv(key value op)#'(lambda(js)(funcall op (jsown:val js key) value)))
(defun fcompare-keys(key1 key2 op)#'(lambda(js)(funcall op (jsown:val js key1) (jsown:val js key2))))

(defun fnth(n-key list-key checks)
  #'(lambda(js)
      (funcall checks
               (nth (jsown:val js n-key) (jsown:val js list-key)) ) )  )

(defun remove-nth(n lst)  (append (subseq lst 0 n) (subseq lst (1+ n) (length lst))))
(defun set-nth(n lst item)  (append (subseq lst 0 n) (list item) (subseq lst (1+ n) (length lst))))
(defparameter *model* (jsown:new-js));; current model

(defparameter *policy* (make-hash-table :test #'equal))
(defparameter *update-policy* ())
(defun policy-selector()
  #'(lambda (available)
      (let* ((items (mapcar #'third available))
             (indexes (gethash (copy-tree items)
                              *policy*
                              (copy-tree items)))
             (selected (alexandria:random-elt indexes)))
        (setf (gethash items *policy*) indexes)
        (push #'(lambda()
                  (push selected (gethash items *policy*)))
              *update-policy*)
        selected )))

(defun simple-selector()
  #'(lambda(ops)(alexandria:random-elt
                 (mapcar #'third ops))) )

(defun generate-sample(&key (model (jsown:new-js))
                         (tasks *tasks*)
                         (path ())
                         (size 100)
                         (prefix '())
                         (selector (policy-selector)));
  (setf *model* (copy-tree model))
  (format t "~%With model:~S" model)
  (if (>= (print(length path)) size)  'pass
      (let ((available (remove-if-not
                        #'(lambda(task)(let ((res (funcall (first task) model)))
                                         ;;(format t "~%~:[skip~;OK~] ~A" res (third task))
                                         res))
                        tasks)))
        (if available
            (let((selected (if prefix
                               (and (member (first prefix) (mapcar #'third available) :test #'equal)
                                    (first prefix))
                               (funcall selector available))))

              (if selected
                (let*((name (print selected))
                      (step-result (funcall (second (find selected available :test #'equal
                                                          :key #'third))
                                            (copy-tree model)))
                      (result (first step-result))
                      (new-model (second step-result))
                      (new-path (list* (cons name result) path)))
                  (format t "~%Processing ~A" name)
                  (cond
                    ((null (second result)) new-path) ;;error detected
                    (t (generate-sample :prefix (rest prefix)
                                        :model new-model
                                        :tasks tasks
                                        :path new-path
                                        :size size))))
                (format t "~%invalid path")))
            (progn (format t "~%Deadend reached!")) ) ) ))


(defun test-random(&key(prefix ()) (size (length prefix)))
  (clear-events)
  (router-new)
  (queue-new)
  (let ((res (generate-sample :tasks *tasks* :size size :prefix prefix)))
    (if (equal res 'pass)
        (progn
          (format t "~%Pass"))
        (progn
          (print-log (list nil nil (reduce #'append (mapcar #'third (mapcar #'rest (reverse res))))))
          (mapcar #'first res)
          )
       ) ) )


(defun find-bug (size )
  (setf *update-policy* ())
  (print(length (test-random :prefix (loop for x = (let((*standard-output* (make-broadcast-stream)))(test-random :size size)) :if x :return (print (reverse x))
                    do (setf *update-policy* ()) (format t "."))))))

(defun scan-bug(size)
  (setf *policy* (make-hash-table :test #'equal))
  (time (loop for max-size = size then (let ((last (find-bug max-size)))
                                         (loop for x from 1 to 3 do (mapcar #'funcall *update-policy*))
                                         (if (< last max-size) (/ (- max-size last) 2)
                                             max-size))
           :repeat 100
           do (format t  "~%-------- max-size ~A" max-size)
             )) )



(defun reduce-test(candidates)
  (when candidates
    (let ((all ())
          (candidate (first candidates)))

      (destructuring-bind (list len) candidate
        (format t "~%-------------------------")
        (format t "~%--> Total:~A Current:~A ~{~S~^, ~}" (length candidates)(length list) list)
        (format t "~%-------------------------")
        (alexandria:map-combinations #'(lambda(steps) (push (reverse(test-random :prefix steps)) all)) list :length len)
        (let((reduced (remove-if #'null all)))
          (if reduced
              (reduce-test (append (mapcar #'(lambda(i)(list i (1- (length i))))reduced) (rest candidates)))
              (list* candidate (reduce-test (rest candidates))) ))))))

;; (defun replay-sample(&key (tasks *tasks*) (path ()) (test-case ()))
;;   (if (null test-case)
;;       'pass
;;       (let*((available (remove-if-not #'(lambda(task)(funcall (first task))) tasks))
;;             (selected (first test-case))
;;             (result (funcall (second (nth selected available))))
;;             (new-path (list* (cons selected result) path)))
;;         (cond
;;           ((null (second result)) new-path);;error detected
;;           (t (replay-sample :tasks tasks :path new-path :test-case (rest test-case))) )) ) )


;; (defun test-case(list)
;;   (clear-events)
;;   (let ((res (replay-sample :tasks *tasks* :test-case list)))
;;     (if (equal res 'pass)
;;         'pass
;;         (progn
;;           (print-log (list nil nil (reduce #'append (mapcar #'third (mapcar #'rest (reverse res))))))
;;           res ) ) ) )
