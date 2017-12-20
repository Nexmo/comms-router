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

(defun tor(step &rest steps)
  #'(lambda()
      (destructuring-bind (result check description)(funcall step)
        (if (or check (null steps)) 
            (list result check description)
            (destructuring-bind (result1 check1 description1) (funcall(apply #'tor steps))
              (list result1 check1 (append description description1)))))))

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

;; bind for model
(defun mstep(step-fn)
  #'(lambda(model)(funcall step-fn)))
(defun mbind(mfn p-mfn)
  #'(lambda(model)
      (funcall (funcall p-mfn (funcall mfn model)) model)))

(defun mand(mfn &rest mfns)
  (if mfns
      #'(lambda(m)
          (funcall (apply #'mand mfns)
                   (funcall mfn m)) )
      mfn) )

(defmacro mlet(vars &body body)
  `(mbind ,(first (last (first vars)) )
          ,(if (butlast(subseq (first vars) 1))
              `(compose
                #'(lambda(,(first (first vars)))
                    ,(if (rest vars)
                         `(mlet ,(rest vars) ,@body)
                         `(progn ,@body)))
                ,@(butlast(subseq (first vars) 1)))
              `#'(lambda(,(first (first vars)))
                   ,(if (rest vars)
                        `(mlet ,(rest vars) ,@body)
                        `(progn ,@body))))))

;;
(defun print-log(result &optional indent)
  (destructuring-bind (result check log)result
    (format t "~{~%---------------~{~%Description:~A~%Result:~:[FAIL~;pass~]~%Request:~A~%Response:~A~%~{Checks:~A~%~}~}~}" log)

    (format t "Result:~:[FAIL~;pass~]" check))
  (second result))

(defun remove-char(pos string)
  (concatenate 'string (subseq string 0 pos) (subseq string (1+ pos))))

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

;;; external storage - in order to use id's instead of unique keys. This way
;;; there will be not equal states that differ only by id value return from api under test
(defparameter *mem* (make-hash-table :test #'equal))
(defun ref-new(prefix &key (id 0) (value nil))
  (unless value
    (break))
  (unless (gethash prefix *mem*)
    (setf (gethash prefix *mem*) (make-hash-table :test #'equal)))
  (if (not (gethash id (gethash prefix *mem*)))
      (progn
        (setf (gethash id (gethash prefix *mem*)) value)
        id)
      (ref-new prefix :id (1+ id) :value value)))
(defun ref (prefix id)
  (gethash id (gethash prefix *mem*)))

(defun ref-id (prefix json)
  (ref prefix (jsown:val json "ref")) )

(defun ref-set(prefix json value)
  (setf (gethash (jsown:val json "ref") (gethash prefix *mem*)) value)
  (jsown:val json "ref"))

(defun ref-del (prefix id)
  (remhash id (gethash prefix *mem*)))


(defparameter *policy* (make-hash-table :test #'equal))
(defparameter *update-policy* ())
(defun policy-selector()
  #'(lambda (available &key(prefer nil))
      (let* ((items (mapcar #'third available))
             (indexes (gethash (copy-tree items)
                              *policy*
                              (copy-tree items)))
             (selected (if prefer prefer (alexandria:random-elt indexes))))
        (setf (gethash items *policy*) indexes)
        (push #'(lambda()
                  (push selected (gethash items *policy*)))
              *update-policy*)
        selected )))

(defun simple-selector()
  #'(lambda(ops &key(prefer nil))
      (if prefer prefer
        (alexandria:random-elt
         (mapcar #'third ops)))) )

(defun generate-sample(&key (model (jsown:new-js))
                         (tasks *tasks*)
                         (path ())
                         (size 100)
                         (prefix '())
                         (selector (rl-policy-selector)));
  (setf *model* (copy-tree model))
  (format t "~%With model:~S max=~A sofar ~A" model size (length path))
  (if (>= (print(length path)) size)  'pass
      (let ((available (remove-if-not
                        #'(lambda(task)(let ((res (funcall (first task) model)))
                                         ;;(format t "~%~:[skip~;OK~] ~A" res (third task))
                                         res))
                        tasks)))
        (if available
            (let((selected (if prefix
                               (and (member (first prefix) (mapcar #'third available) :test #'equal)
                                    (funcall selector available :prefer (first prefix)))
                               (funcall selector available))))
              (format t "~%Selected:~A"selected)
              (if selected
                (let*((name selected)
                      (step-result (funcall (second (find selected available :test #'equal
                                                          :key #'third))
                                            (copy-tree model)))
                      (result (first step-result))
                      (new-model (second step-result))
                      (new-path (list* (cons name result) path)))
                  ;;(format t "~%Processing ~A" name)
                  (cond
                    ((null (second result)) (progn ;;(funcall selector (list))
                                                   new-path)) ;;error detected
                    (t (generate-sample :prefix (rest prefix)
                                        :model new-model
                                        :tasks tasks
                                        :path new-path
                                        :size size
                                        :selector selector))))
                (format t "~%invalid path")))
            (progn (format t "~%Deadend reached!")) ) ) ))

(defun test-random(&key(prefix ()) (size (length prefix)) (selector (simple-selector);;(policy-selector)
                                                                    ))
  (let* (;;(router-id (jsown:val (router-new) "ref"))
         ;;(queue-id (jsown:val (queue-new :router-id router-id) "ref"))
         (*mem* (make-hash-table :test #'equal))
         (res (generate-sample :model (jsown:new-js ;;("router" router-id)
                                                    ;;("queue" queue-id)
                                                    )
                               :selector selector
                               :tasks *tasks* :size size :prefix prefix)))
    (if (equal res 'pass)
        (progn
          (format t "~%Pass"))
        (progn
          (print-log (list nil nil (reduce #'append (mapcar #'third (mapcar #'rest (reverse res))))))
          (mapcar #'first res) ) ) ) )

(defun find-bug (size &key (threads 1) (prefixes '()))
  (setf *update-policy* ())
  (let ((problem-cases (loop for x = (let ((fn #'(lambda(i)
                                                   (let*((res nil)
                                                         (str (with-output-to-string(s)
                                                                (let((*standard-output* s))
                                                                  (setf res (test-random :prefix (when prefixes (random-elt prefixes )) :size size))))))
                                                     (unless (null res)
                                                       (format t "~A" str))
                                                     res ))))
                                       (if (= threads 1)
                                           (list (funcall fn 1))
                                           (lparallel:pmapcar fn (loop :repeat threads :collect 1))) )
                          :if (some (complement #'null) x )
                          :return (let ((res (mapcar #'reverse x)))
                                    (format t "~%Failed cases:~%")
                                    (print (mapcar #'reverse x))
                                    res)
                          do 
                            ;(setf *update-policy* ())
                            (format t "."))))
    (print(reduce #'min (print (remove-if #'zerop (mapcar #'length (let ((fn #'(lambda(problem-case)(test-random :prefix problem-case))))
                                                                     (if (= (length problem-cases) 1 )
                                                                         (list (funcall fn (first problem-cases)))
                                                                         (lparallel:pmapcar fn problem-cases)) ))))
                  :initial-value size)) ) )

(defun reduce-test(size &key (threads 10))
  (loop for last = size then (let ((len (find-bug last :threads threads))) (if (zerop len) last len)) do
       (format t "~%>---------------------------------------- ~A ------------------------" last)))

'(loop :while (null (test-random :prefix '("create router" "create queue" "create task when there are no ready agents"
                                           "create agent" "set-agent ready if there are waiting tasks"
                                           "complete task when there are no waiting tasks"))))
