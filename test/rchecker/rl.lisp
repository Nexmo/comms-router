(in-package :rchecker)

(defun r(state)
  (cond ((equal state '(4 3)) 100)
        ((equal state '(4 2)) -100)
        ((null (and (jsown:keyp state "result") (not (jsown:val state "result")))) 100)
        (t -0.1)))

(defparameter *utility* (make-hash-table :test #'equal))
(defparameter *map* (make-hash-table :test #'equal))

(defun u(state) (gethash state *utility* (random 10)))

(defun next-state(state action)
  (gethash action (gethash state *map* (make-hash-table :test #'equal)) :na))

(defun state-actions(state)
  (let ((actions ()))
    (maphash #'(lambda(k v)(push k actions)) (gethash state *map* (make-hash-table :test #'equal)))
    actions) )

(defun rl-policy-selector()
  (let ((previous-state ())
        (previous-action ())
        (n 0)
        (gamma 0.98))
    #'(lambda (available-actions &key prefer (model-raw *model*))
        (let ((available (mapcar #'third available-actions))
              (model (jsown:remkey
                      (jsown:remkey
                       (copy-tree model-raw)
                       "queue")
                      "router")))
          (when previous-state
            (unless (gethash previous-state *map*)
              (setf (gethash previous-state *map*) (make-hash-table :test #'equal)))
            (when (and (gethash previous-action (gethash previous-state *map*))
                       (not (equal (gethash previous-action (gethash previous-state *map*))
                                   model)))
              (format t "~%!!!!!!!! ~S -> ~S -> ~S and ~S" previous-state previous-action
                      (gethash previous-action (gethash previous-state *map*))
                      model )
              (break))
            ;;(format t "~%Previous action ~S" previous-action)
            (setf (gethash previous-action (gethash previous-state *map*)) (copy-tree model)))
          (let* ((best (first
                        (reduce #'(lambda(current action)
                                    (destructuring-bind (max-action max-u) current
                                      (let ((u (u (next-state model action))))
                                        (format t "~%~A -> ~A" action u)
                                        (if (> u max-u)
                                            (list action u)
                                            current))))
                                available :initial-value (list "none" -1))))
                 (selected (progn
                             (if prefer prefer
                                 (if (> (random 10) 8)
                                     (alexandria:random-elt available)
                                     best))
                             best)))

            (setf previous-state (copy-tree model)
                  previous-action selected
                  n (1+ n))

            (unless (gethash model *utility*)
              ;;(format t "~%Setting utility ~S = ~A" model (r model))
              (setf (gethash model *utility*)(r model)))

            (format t "~%RL selected ~A" selected)
            selected) ) )))

(defun test-failure(model)
  (funcall (tstep-result "Failed state reached" model (and (not(equal model '(4 3)))
                                                          (not(equal model '(4 2)))) "Failed state reached" nil)) )

(defparameter *test-model*
  (list
   (action "up" #'(lambda(model) (and (> 3 (second model))
                                      (not (equal model (list 2 1)))))
           #'(lambda(model)
               #'(lambda(model)(funcall (test-failure model))))
           #'(lambda(res model)
               (list (first model) (1+ (second model)))))
   (action "down" #'(lambda(model) (and (> (second model) 1 )
                                      (not (equal model (list 2 3)))))
           #'(lambda(model)
               #'(lambda(model)(funcall (test-failure model))))
           #'(lambda(res model)
               (list (first model) (1- (second model)))))

   (action "left" #'(lambda(model) (and (> (first model) 1 )
                                        (not (equal model (list 3 2)))))
           #'(lambda(model)
               #'(lambda(model)(funcall (test-failure model))))
           #'(lambda(res model)
               (list (1- (first model)) (second model))))

   (action "right" #'(lambda(model) (and (< (first model) 4)
                                        (not (equal model (list 1 2)))))
           #'(lambda(model)
               #'(lambda(model)(funcall (test-failure model))))
           #'(lambda(res model)
               (list (1+ (first model)) (second model))))))

(setf *update-policy* nil)


(setf *map* (make-hash-table :test #'equal))
(setf *utility* (make-hash-table :test #'equal))

(defun iterate-model(&key (prefix ()) (size 1000) (tasks *tasks*))
  ;;(setf *utility* (make-hash-table :test #'equal))
  (print (length (loop for x = (let* ((router-id (jsown:val (router-new) "ref"))
                                      (queue-id (jsown:val (queue-new :router-id router-id) "ref")) )
                                 (setf *mem* (make-hash-table :test #'equal))
                                 (generate-sample :prefix prefix :model (jsown:new-js ("router" router-id) ("queue" queue-id))
                                                  :size (if prefix (length prefix) size) :selector (rl-policy-selector) :tasks tasks))
                    :if (and (listp x) x) :return x)))
  (loop for (new-u err) = (multiple-value-list (value-iteration :utility *utility*  :gamma 0.8)) :while (> err (* 0.001 (/ (1-0.8) 0.8)))
     do (setf *utility* new-u)))

(defun value-iteration(&key(utility *utility*) (gamma 0.8))
  (let((new-u (make-hash-table :test #'equal))
       (max-err 0))
    (maphash #'(lambda(k v)
                 ;;(format t "~%processing ~A ~A" k v)
                 (let ((old v)
                       (new (+ (r k) (* gamma
                                        (reduce #'max (mapcar
                                                       #'(lambda(selected) (let ((u (u (next-state k selected))))
                                                                             ;(format t "~%~A -> ~A ~A" k selected u)
                                                                             u ))
                                                       (state-actions k))
                                                :initial-value 0)))))
                   (setf (gethash k new-u) new)
                   (setf max-err (max max-err (abs (- new old)))) ) ) utility)
    (values new-u max-err)))
