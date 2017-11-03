(in-package :rchecker)

(defun r(state)
  (cond ((equal state '(4 3)) 100)
        ((equal state '(4 2)) -100)
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
    #'(lambda (available-actions &key (model *model*))
        (let ((available (mapcar #'third available-actions)))
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
            (setf (gethash previous-action (gethash previous-state *map*)) (copy-tree model)))
          (let* ((best (first
                       (reduce #'(lambda(current action)
                                   (destructuring-bind (max-action max-u) current
                                     (let ((u (u (next-state model action))))
                                       (if (> u max-u)
                                           (list action u)
                                           current))))
                               available :initial-value (list "none" -1))))
                (selected (if (> (random 10) 11)
                              (alexandria:random-elt available)
                              best)))

            (setf previous-state (copy-tree model)
                  previous-action selected
                  n (1+ n))

            (unless (gethash model *utility*)
              (setf (gethash model *utility*)(r model)))

            ;;(format t "~%RL selected ~A" selected)
            selected) ) )))

(defun test-failure(model)
  (funcall (tstep-result "Failed state reached" model (and (not(equal model '(4 3)))
                                                          (not(equal model '(4 2)))) "Failed state reached" nil)) )

(defparameter *test-model*
  (list
   (action "up" #'(lambda(model) (and (> 3 (second model))
                                      (not (equal model (list 2 1)))))
           #'(lambda(model)
               (list (test-failure model) (list (first model) (1+ (second model))))))
   (action "down" #'(lambda(model) (and (> (second model) 1 )
                                      (not (equal model (list 2 3)))))
           #'(lambda(model)
               (list (test-failure model) (list (first model) (1- (second model))))))

   (action "left" #'(lambda(model) (and (> (first model) 1 )
                                        (not (equal model (list 3 2)))))
           #'(lambda(model)
               (list (test-failure model) (list (1- (first model)) (second model)))))

   (action "right" #'(lambda(model) (and (< (first model) 4)
                                        (not (equal model (list 1 2)))))
           #'(lambda(model)
               (list (test-failure model) (list (1+ (first model)) (second model)))))))

(setf *update-policy* nil)
(setf *policy* nil)
(defun iterate-model(&key (size 1) (tasks *tasks*) )
  (setf *utility* (make-hash-table :test #'equal))
  (loop for x from 0 to size do
       (print (length (generate-sample :model '(1 1) :size 1000 :selector (rl-policy-selector) :tasks tasks)))
       (loop for (new-u err) = (multiple-value-list (value-iteration :utility *utility*  :gamma 0.8)) :while (> err 0.001)
          do (setf *utility* new-u)) ) )


(defun value-iteration(&key(utility *utility*) (gamma 0.8))
  (let((new-u (make-hash-table :test #'equal))
       (max-err 0))
    (maphash #'(lambda(k v)
                 (let ((old v)
                       (new (+ (r k) (* gamma
                                        (reduce #'max (mapcar
                                                       #'(lambda(selected) (let ((u (u (next-state k selected))))
                                                                             ;;(format t "~%~A -> ~A ~A" k selected u)
                                                                             u ))
                                                       (state-actions k))
                                                :initial-value 0)))))
                   (setf (gethash k new-u) new)
                   (setf max-err (max max-err (abs (- new old)))) ) ) utility)
    (values new-u max-err)))
