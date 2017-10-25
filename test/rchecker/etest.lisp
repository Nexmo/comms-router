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
                                                     (list "Waiting step to complete has failed"))))))) ) ))

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
            (apply #'fand checks)))
      check))

(defun fnot(check)
  #'(lambda(js)
      (not (funcall check js))))

(defun for(check &rest checks)
  (if checks
      #'(lambda(js)
          (if (funcall check js) t
              (apply #'for checks)))
      check))

(defun fhas-json() #'(lambda(js)(and (listp json) (not (null json)))))
(defun fhas-key(key) #'(lambda(js)(jsown:keyp js key)))
(defun fhas-kv(key value op)#'(lambda(js)(funcall op (jsown:val js key) value)))
(defun fcompare-keys(key1 key2 op)#'(lambda(js)(funcall op (jsown:val js key1) (jsown:val js key2))))

(defun remove-nth(n lst)  (append (subseq lst 0 n) (subseq lst (1+ n) (length lst))))

(defparameter *tasks*
  (list
   (list (for
          (fnot (fhas-key "tasks"))
          (fand (fhas-kv "tasks" 10 #'(lambda(js-val val)(< (length js-val) val)))))
         #'(lambda(model)(let ((res (funcall (etask-new))))
                           (list res (if (second res)
                                         (jsown:extend-js (copy-tree model)
                                           ("tasks" (list* (first res) (when (jsown:keyp model "tasks")
                                                                         (jsown:val model "tasks")))))
                                         model))))
         "create task")
   (list (fand (fhas-key "tasks")
               (fhas-kv "tasks" 0 #'(lambda(js-val val)(> (length js-val) val))))
         #'(lambda(model)(let ((res (funcall (step-result 0 t "Select the latest task."))))
                           (list res (if (second res)
                                         (jsown:extend-js (copy-tree model) ("selected-task" 0))
                                         model))))
         "select last task")
   (list (fand (fhas-key "tasks")
               (fhas-key "selected-task")
               (fcompare-keys "selected-task" "tasks" #'(lambda(selected tasks)(< (1+ selected) (length tasks)))))
         #'(lambda(model)(let* ((next (1+ (jsown:val model "selected-task")))
                                (res (funcall (step-result next t "Select previous task."))))
                           (list res (if (second res)
                                         (jsown:extend-js (copy-tree model) ("selected-task" next))
                                         model))))
         "select next task")

   (list (fhas-key "selected-task")
         #'(lambda(model)(let* ((selected (jsown:val model "selected-task"))
                                (tasks (jsown:val model "tasks"))
                                (res (funcall (etask-del :id (jsown:val (nth selected tasks) "id")))))
                           (list res (if (second res)
                                         (jsown:remkey (jsown:extend-js(copy-tree model)
                                                         ("tasks" (remove-nth selected tasks)))
                                                       "selected-task")
                                         model))))
         "delete task")))

(defun generate-sample(&key (model (jsown:new-js)) (tasks *tasks*) (path ()) (size 100))
  (if (>= (length path) size)  'pass
      (let*((available (remove-if-not #'(lambda(task)(format t "~%checking ~A"(third task))(funcall (first task) model)) tasks))
            (selected (random (length available)))
            (name (print (third (nth selected available))))
            (step-result (funcall (second (nth selected available))  model))
            (result (first step-result))
            (new-model (second step-result))
            (new-path (list* (cons selected result) path)))
        (format t "~%Processing ~A" name)
        (cond
          ((null (second result)) new-path);;error detected
          (t (generate-sample :model new-model :tasks tasks :path new-path :size size)) )) )  )

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

;; (defun test-random(&key(size 100))
;;   (clear-events)
;;   (let ((res (generate-sample :tasks *tasks* :size size)))
;;     (unless (equal res 'pass)
;;       (print-log (list nil nil (reduce #'append (mapcar #'third (mapcar #'rest (reverse res))))))
;;       (mapcar #'first res) ) ) )

;; (defun test-case(list)
;;   (clear-events)
;;   (let ((res (replay-sample :tasks *tasks* :test-case list)))
;;     (if (equal res 'pass)
;;         'pass
;;         (progn
;;           (print-log (list nil nil (reduce #'append (mapcar #'third (mapcar #'rest (reverse res))))))
;;           res ) ) ) )
;; (defun find-bug (size)
;;   (loop for x = (test-random :size size) :repeat 1000 :if x :return x))
