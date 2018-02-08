;;;; rchecker.asd

(asdf:defsystem #:rchecker
  :description "Describe rchecker here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:jsown
               #:drakma
               #:dexador
               #:hunchentoot
               #:lparallel
	       #:do-urlencode
               #:queues.simple-cqueue
               #:check-it)
  :serial t
  :components ((:file "package")
               (:file "transport")
               (:file "api")
               (:file "rchecker")
               (:file "eapi")
               (:file "etest")
               (:file "emodel")
               (:file "auto-complete")
               (:file "tests/task-queue")))
