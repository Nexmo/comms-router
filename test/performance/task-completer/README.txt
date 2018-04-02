This is fast auto-completer server
Run it with:
sbcl
(ql:quickload :task-completer)

It will accept task callbacks on port 5000 and will set status of the task in the callback to "completed"
