(in-package #:rchecker)
(setf lparallel:*kernel* (lparallel:make-kernel 10))

;;; "rchecker" goes here. Hacks and glory await!

(defun has-key(key)
  #'(lambda(json)
      (if (member key (jsown:keywords json) :test #'equal)
          (list t (list(format nil "ok - result contains key ~S" key)))
          (list nil (list(format nil "FAIL- ~S should have key ~S" json key))))))

(defun has-keys(key)
  #'(lambda(json)
      (if (not (zerop(length (jsown:filter json map key))))
          (list t (list(format nil "ok - result contains list of key ~S" key)))
          (list nil (list(format nil "FAIL- ~S should have at least one key ~S" json key))))))

(defun not-contains(text)
  #'(lambda(data)
      (if (search text member key (jsown:keywords json) :test #'equal)
          (list t (list (format nil "OK - result contains key ~S" text)))
          (list nil (list (format nil "FAIL - ~S should not contain ~S" data text))))))

(defun is-equal(text)
  #'(lambda(data)
      (if (equal text data)
          (list t (list (format nil "OK - result equals to ~S" text)))
          (list nil (list (format nil "FAIL - ~S should be equal to ~S" data text))))))

(defun check-result(check descr)
  #'(lambda(json)(list check descr)))

(defun check-bind (check-fn rcheck-fn)
  #'(lambda(json)
      (destructuring-bind (check descr) (funcall check-fn json)
        (if check (funcall rcheck-fn descr)
            (list nil descr)))))

(defun check-and(check-fn &rest checks)
  (if checks
      #'(lambda(json)
          (funcall (check-bind
                    check-fn
                    #'(lambda(descr)
                        (destructuring-bind (res descr1) (funcall (apply #'check-and checks) json)
                          (list res (append descr1 descr)))))
                   json))
      check-fn))

(defun step-result(result status descr)
  #'(lambda()(list result status descr)))

(defun check-step (thunk-fn check-fn)
  #'(lambda()
      (let((res (funcall thunk-fn)))
        (list* res (funcall check-fn res))) ) )

(defun step-bind(step-fn p-step-fn)
  #'(lambda()
      (destructuring-bind (result status descr) (funcall step-fn)
        (print status)
        (if status
            (funcall (funcall p-step-fn result descr))
            (list result status descr)))))

(defun step-seq(step &rest steps)
  (if steps
      #'(lambda()
          (destructuring-bind (result status descr) (funcall step)
            (if status
                (destructuring-bind (r s d) (funcall (apply #'step-seq steps))
                  (list r s (append d descr)))
                (list result status descr))))
      step))

(defun check-routers(x)
  (step-bind
   (check-step #'(lambda()(router-new :name x)) (has-key "id"))
   (lambda(json descr)
     (destructuring-bind (result status descr1)
         (funcall (check-step #'(lambda()
                                  (router-del :id (jsown:val json "id")))
                              (is-equal "")))
       (step-result result status (append descr1 descr))))))

(defun checker(retry)
  #'(lambda(x)
      (loop for results = (lparallel:pmapcar #'(lambda(x)(funcall (check-routers x))) x)
         for res = (every #'second results)
         for try from 1 to retry :if res :return res
         :if (= try retry) :return nil do
           (format t "~%!!!!!!!! (~S). Will retry~%" results)

           (sleep 1))))

'(check-it (generator (list (integer) :max-length 10)) (checker 3))

(defun crouter-new()
  (check-step #'(lambda()(router-new))
              (has-key "id")))

(defun crouter()
  (check-step #'(lambda()(router))
              (has-key "id")))
(defun crouter-all()
  (check-step #'(lambda()(router-all))
              (has-keys "id")))

(defun crouter-update()
  (check-step #'(lambda()(router-set))
              (is-equal "")))
(defun crouter-del()
  (check-step #'(lambda()(router-del))
              (is-equal "")))

(defun crouter-recreate()
  (step-bind (check-step #'(lambda()
                             (router-del))
                         (is-equal ""))
             (lambda(json descr)
               (destructuring-bind (result status descr1)
                   (funcall (check-step #'(lambda()(router-new))
                                        (has-key "id")))
                 (step-result result status (append descr1 descr))))))
;;; agent
(defun cagent-new()
  (check-step #'(lambda()(agent-new))
              (has-key "id")))

(defun cagent()
  (check-step #'(lambda()(agent))
              (has-key "id")))
(defun cagent-all()
  (check-step #'(lambda()(agent-all))
              (has-keys "id")))

(defun cagent-update()
  (check-step #'(lambda()(agent-put))
              (is-equal "")))

(defun cagent-del()
  (check-step #'(lambda()(agent-del))
              (is-equal "")))

(defun cagent-recreate()
  (step-bind (check-step #'(lambda()
                             (agent-del))
                         (is-equal ""))
             (lambda(json descr)
               (destructuring-bind (result status descr1)
                   (funcall (check-step #'(lambda()(agent-new))
                                        (has-key "id")))
                 (step-result result status (append descr1 descr))))))
;;;
;;; queue
(defun cqueue-new-and-bind-to-agent()
  (check-step #'(lambda()(queue-new))
              (check-and (has-key "id")
                         #'(lambda(json) (bind-agent-to-queue) (list nil '("bind agent to queue"))))))

(defun cqueue-new()
  (check-step #'(lambda()(queue-new))
              (has-key "id")))

(defun cqueue()
  (check-step #'(lambda()(queue))
              (has-key "id")))

(defun cqueue-all()
  (check-step #'(lambda()(queue-all))
              (has-keys "id")))

(defun cqueue-update()
  (check-step #'(lambda()(queue-put))
              (is-equal "")))

(defun cqueue-del()
  (check-step #'(lambda()(queue-del))
              (is-equal "")))

(defun cqueue-recreate()
  (step-bind (check-step #'(lambda()
                             (queue-del))
                         (is-equal ""))
             (lambda(json descr)
               (destructuring-bind (result status descr1)
                   (funcall (check-step #'(lambda()(queue-new))
                                        (has-key "id")))
                 (step-result result status (append descr1 descr))))))

;;;
;;; plan
(defun cplan-new()
  (check-step #'(lambda()(plan-new :rules ()))
              (has-key "id")))

(defun cplan()
  (check-step #'(lambda()(plan))
              (has-key "id")))
(defun cplan-all()
  (check-step #'(lambda()(plan-all))
              (has-keys "id")))

(defun cplan-update()
  (check-step #'(lambda()(plan-put :rules '()))
              (is-equal "")))

(defun cplan-del()
  (check-step #'(lambda()(plan-del))
              (is-equal "")))


;;;
;;; task
(defun ctask-new()
  (check-step #'(lambda()(task-new))
              (has-key "id")))

(defun ctask()
  (check-step #'(lambda()(task))
              (has-key "id")))
(defun ctask-all()
  (check-step #'(lambda()(task-all))
              (has-keys "id")))

(defun ctask-update()
  (check-step #'(lambda()(task-put))
              (is-equal "")))

(defun ctask-del()
  (check-step #'(lambda()(task-del))
              (is-equal "")))

(defun ctask-recreate()
  (step-bind (check-step #'(lambda()
                             (task-del))
                         (is-equal ""))
             (lambda(json descr)
               (destructuring-bind (result status descr1)
                   (funcall (check-step #'(lambda()(task-new))
                                        (has-key "id")))
                 (step-result result status (append descr1 descr))))))

(defun check-ops(all)
  (cond
    ((null all) t)
    ((listp all) (when (check-ops (first all))
                   (check-ops (rest all))))
    (t (let((res (funcall (funcall all))))
         (unless res
           (print res))
         (second res) )) ))

(def-generator generator-crouter()
  (list (or 'crouter 'crouter-update)))

(defun test-router()
  (check-it (generator
             (tuple
              'crouter-new
              (list
               (tuple
                'cagent-new
                'cqueue-new-and-bind-to-agent
                (or
                 (tuple 'ctask-new
                        (list (tuple 'ctask-update 'cagent-update))
                        (list 'ctask-del :max-length 1))
                 'crouter 'crouter-update
                 'cqueue 'cqueue-update
                 'cagent )
                (list 'cqueue-del :max-length 1)
                (list 'cagent-del :max-length 1)))
              (list 'crouter-del :max-length 1))) #'check-ops))

(defun crud (resource)
  (apply #'step-seq (mapcar #'(lambda(name)(funcall (symbol-function (intern name))))
                            (list (format nil "~A-NEW" resource)
                                  (format nil "~A" resource)
                                  (format nil "~A-ALL" resource)
                                  (format nil "~A-UPDATE" resource)
                                  (format nil "~A-DEL" resource) ))))

(defun crud-router()  (crud 'crouter)  )
(defun crud-queue()
  (step-seq (crouter-new) (crud 'cqueue)))
(defun crud-agent()
  (step-seq (crouter-new) (crud 'cagent)))
(defun crud-task()
  (step-seq (crouter-new) (cqueue-new) (crud 'cqueue)))
(defun crud-plan()
  (step-seq (crouter-new)(cqueue-new) (crud 'cplan)))

(defun crud-all()
  (step-seq (crud-router) (crud-queue) (crud-agent) (crud-task) (crud-plan)))
