;;;; task-completer.asd

(asdf:defsystem #:task-completer
  :description "Describe task-completer here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma-async
               #:jsown
               #:woo)
  :serial t
  :components ((:file "package")
               (:file "task-completer")))

