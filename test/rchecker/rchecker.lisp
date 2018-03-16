(in-package #:rchecker)
(setf lparallel:*kernel* (lparallel:make-kernel 10))

(defun has-json()
  #'(lambda(json)
      (if (and (listp json) (not (null json)))
          (list t (list (format nil "ok - Response is json")))
          (list nil (list (format nil "FAIL- Response should be json, not ~A."json))))))

(defun has-kv(key value &optional (compare-fn #'equal))
  #'(lambda(json)
      (if (member key (jsown:keywords json) :test #'equal)
          (if (funcall compare-fn (jsown:val json key) value)
              (list t (list(format nil "ok - result contains key ~S=~S" key value)))
              (list nil (list(format nil "FAIL- key ~S should be ~S but it is ~S" key value (jsown:val json key)))) )
          (list nil (list(format nil "FAIL- should have key ~A in ~S" key (jsown:to-json json)))))))

(defun publish-id(resource)
  #'(lambda(js)
      (funcall (fire-event resource) (jsown:val js "ref"))
      (list t (list(format nil "ok - publish ~A id ->~A" resource (jsown:val js "ref"))))))

(defun remove-id(resource)
  #'(lambda(js)
      (clear-event resource)
      (list t (list(format nil "ok - remove resource ~A id" resource)))))

(defun has-key(key)
  #'(lambda(json)
      (if (member key (jsown:keywords json) :test #'equal)
          (list t (list(format nil "ok - result contains key ~S" key)))
          (list nil (list(format nil "FAIL- ~A should have key ~S" (jsown:to-json json) key))))))

(defun has-keys(key)
  #'(lambda(json)
      (if (not (zerop(length (jsown:filter json map key))))
          (list t (list(format nil "ok - result contains list of key ~S" key)))
          (list nil (list(format nil "FAIL- ~S should have at least one key ~S" json key))))))

(defun not-contains(text)
  #'(lambda(data)
      (if (not (search text (format nil "~S" data)))
          (list t (list (format nil "OK - result not contains text ~S" text)))
          (list nil (list (format nil "FAIL - ~S should not contain ~S" data text))))))

(defun contains(text)
  #'(lambda(data)
      (if (search text (format nil "~S" data))
          (list t (list (format nil "OK - result contains text ~S" text)))
          (list nil (list (format nil "FAIL - ~S should contain ~S" data text))))))

(defun is-equal(text)
  #'(lambda(data)
      (if (equal text data)
          (list t (list (format nil "OK - result equals to ~S" text)))
          (list nil (list (format nil "FAIL - ~S should be equal to ~S" data text))))))

(defun js-val (key)
  #'(lambda(json) (jsown:val json key)))

(defun js-val-or (key &key default)
  #'(lambda(json) (if (jsown:keyp json key)
                      (jsown:val json key)
                      default)))

(defun js-remkey (key)
  #'(lambda(json) (jsown:remkey (copy-tree json) key)))

(defun js-extend (&rest kvals)
  #'(lambda(json)
      (let ((new-json (copy-tree json)))
        (loop for (k v) in kvals do
             (setf (jsown:val new-json k) v) )
        new-json)))

(defun js-push (key value)
  #'(lambda(json)
      (let ((new-json (copy-tree json)))
        (jsown:extend-js new-json
          (key (list* value
                      (when (jsown:keyp new-json key)
                        (jsown:val new-json key)))))
        new-json)))

(defun js-selected(key list)
  "return nth element from json list where key and list are key in json object"
  #'(lambda(json) (nth (jsown:val json key) (jsown:val json list))))

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
   (check-step #'(lambda()(router-new :name x)) (has-key "ref"))
   (lambda(json descr)
     (destructuring-bind (result status descr1)
         (funcall (check-step #'(lambda()
                                  (router-del :id (jsown:val json "ref")))
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
              (has-key "ref")))

(defun crouter()
  (check-step #'(lambda()(router))
              (has-key "ref")))
(defun crouter-all()
  (check-step #'(lambda()(router-all))
              (has-keys "ref")))

(defun crouter-update()
  (check-step #'(lambda()(router-set))
              (is-equal "")))
(defun crouter-put()
  (check-step #'(lambda()(router-put :id (get-id "router-")))
              (has-key "ref")))
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
                                        (has-key "ref")))
                 (step-result result status (append descr1 descr))))))
;;; agent
(defun cagent-new()
  (check-step #'(lambda()(agent-new))
              (has-key "ref")))

(defun cagent()
  (check-step #'(lambda()(agent))
              (has-key "ref")))
(defun cagent-all()
  (check-step #'(lambda()(agent-all))
              (has-keys "ref")))

(defun cagent-update()
  (check-step #'(lambda()(agent-put))
              (has-key "ref")))

(defun cagent-set()
  (check-step #'(lambda()(agent-set :state "ready"))
              (is-equal "")))

(defun cagent-ready()
  (check-step #'(lambda()(agent))
              (has-kv "state" "ready")))

(defun cagent-busy()
  (check-step #'(lambda()(agent))
              (has-kv "state" "busy")))

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
                                        (has-key "ref")))
                 (step-result result status (append descr1 descr))))))
;;;
;;; queue
(defun cqueue-new-and-bind-to-agent()
  (check-step #'(lambda()(queue-new))
              (check-and (has-key "ref")
                         #'(lambda(json) (bind-agent-to-queue) (list nil '("bind agent to queue intentionally fail"))))))

(defun cqueue-new()
  (check-step #'(lambda()(queue-new))
              (has-key "ref")))

(defun cqueue()
  (check-step #'(lambda()(queue))
              (has-key "ref")))

(defun cqueue-all()
  (check-step #'(lambda()(queue-all))
              (has-keys "ref")))

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
                                        (has-key "ref")))
                 (step-result result status (append descr1 descr))))))

;;;
;;; plan
(defun cplan-new-empty()
  (check-step #'(lambda()(plan-new :rules ()))
              (has-key "ref")))
(defun cplan-new(&key (predicate "true"))
  (check-step #'(lambda()(plan-new :predicate predicate))
              (has-key "ref")))

(defun cplan()
  (check-step #'(lambda()(plan))
              (has-key "ref")))
(defun cplan-all()
  (check-step #'(lambda()(plan-all))
              (has-keys "ref")))

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
              (has-key "ref")))

(defun cptask-new()
  (check-step #'(lambda()(ptask-new))
              (has-key "ref")))

(defun ctask()
  (check-step #'(lambda()(task))
              (has-key "ref")))
(defun ctask-all()
  (check-step #'(lambda()(task-all))
              (has-keys "ref")))

(defun ctask-update()
  (check-step #'(lambda()(task-put))
              (has-key "ref")))

(defun ctask-set()
  (check-step #'(lambda()(task-set :state "completed"))
              (is-equal "")))
(defun ctask-waiting()
  (check-step #'(lambda()(task))
              (has-kv "state" "waiting")))
(defun ctask-assigned()
  (check-step #'(lambda()(task))
              (has-kv "state" "assigned")))
(defun ctask-completed()
  (check-step #'(lambda()(task))
              (has-kv "state" "completed")))

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
                                        (has-key "ref")))
                 (step-result result status (append descr1 descr))))))

(defun check-ops(all)
  (cond
    ((null all) t)
    ((listp all) (when (check-ops (first all))
                   (check-ops (rest all))))
    (t (let((res (print(funcall (funcall all)))))
         (unless res
           (print res))
         (second res) )) ))

(def-generator generator-crouter()
  (list (or 'crouter 'crouter-update)))

(defun match-task()
#'(lambda(expr)
    (funcall #'values
             (rest
              (funcall (step-seq (crouter-new)
                                 (cqueue-new)
                                 (cplan-new :predicate (format nil "~{~A~}" (flatten expr)))
                                 (cptask-new)
                                 (ctask-del)
                                 (cplan-del)
                                 (cqueue-del)
                                 (crouter-del)))))))

(defun test-task-queue()
  (let*((simple (generator (or "1" "1==1" "1!=0" "1<2" "2>1" "true" "IN(1,[2,1,3])" "HAS([1,2,3],2)")))
        (composite (generator (tuple "(" (tuple simple
                                                (list (tuple (or "&&" "||") simple))
                                                ) ")"))))
    (check-it (generator(tuple simple
                               (list (tuple (or "&&" "||") simple)) ))
              (match-task))))
(def-generator rsql-or ()
  (let*((r-value (generator (or "one" "'two'" "1" "\"1\"")))
        (r-list (generator (tuple "(" r-value (list (tuple "," r-value)) ")")))
        (r-args (generator (or r-list r-value)))
        (r-compare (generator (or "==" "!=" "=lt=" "<" "=le=" "<=" "=gt=" ">"
                                  "=ge=" ">="  "=in=" "=out="))) ;; 
        (r-compare-list (generator (or "=in=" "=out=")))
        (r-selector  (generator (or "language" "department" "s" "not_reserved")))
        (r-comparison (generator ;(tuple r-selector r-compare r-args)
                       (or (tuple r-selector r-compare r-value)
                           (tuple r-selector r-compare-list r-list))))
        (r-group (generator (tuple "(" (rsql-or) ")")))
        (r-constraint (generator (or r-group r-comparison)))
        (r-and (generator (tuple r-constraint (list (tuple ";" r-constraint))))))
    (generator (tuple r-and (list (tuple "," r-and))))))

(defun test-task-rsql()
  (let ((*list-size* 2)) (check-it (generator (rsql-or)) (match-task))))

(defun test-router()
  (check-it (generator
             (tuple
              'crouter-new
              (list
               (tuple
                'cagent-new
                ;;'cqueue-new-and-bind-to-agent
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

(defun test-task-path()
  (check-it (generator
             (tuple
              'crouter-new
              (list
               (tuple
                (or (tuple 'cagent-new 'cqueue-new 'ctask-new)
                    (tuple 'cqueue-new (or (tuple 'ctask-new 'cagent-new)
                                           (tuple 'cagent-new 'ctask-new))) )
                'cagent-set
                (list (or 'ctask-assigned 'cagent-busy) :max-length 2)
                (list (tuple  'ctask-set (list 'cagent-set :max-length 1)
                              (list (or 'ctask-completed'cagent-ready) :max-length 2)
                              )
                      :max-length 1)
                )
               :max-length 1)
               )) #'check-ops))


(def-generator model ()
  (let ((routers (router-all)))
    (if routers
        (generator (or `(router-del :id ,(get-event :router))
                       `(router-del :id ,(get-event :router))))
        (generator (or '(crouter-new) '(crouter-put))))))

(defun test-model()
  (check-it (generator
             (chain ((router (or 'crouter-new 'crouter-put)))
                    (generate
                     (generator (tuple router
                                       (or (chain
                                            ((queue (or 'cqueue-new 'cqueue-put))
                                             (agent (or 'cagent-new 'cagent-put)))
                                            (generate(generator (tuple
                                                                 router
                                                                 (or (tuple queue agent) (tuple agent queue))
                                                                 (or nil 'cqueue-del)
                                                                 (or nil 'cagent-del)))))
                                           'crouter-all
                                           'crouter-del)))))) #'check-ops))

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
