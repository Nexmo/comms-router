;;;; rchecker.asd

(asdf:defsystem #:rchecker
  :description "Describe rchecker here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:jsown
               #:drakma
               #:lparallel
               #:check-it)
  :serial t
  :components ((:file "package")
               (:file "transport")
               (:file "api")
               (:file "rchecker")
               (:file "eapi")
               (:file "etest")
               (:file "tests/task-queue")))
