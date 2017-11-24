;;;; demo-helper.asd

(asdf:defsystem #:demo-helper
  :description "Describe demo-helper here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:lparallel
               #:jsown
               #:cl-who
               #:rchecker)
  :serial t
  :components ((:file "package")
               (:file "demo-helper")))
