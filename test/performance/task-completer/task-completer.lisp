;;;; task-completer.lisp

(in-package #:task-completer)
(defun server()
  (as:tcp-server 
   nil 5000
   (let ((sockets (make-hash-table :test #'equal))
         (requests 0)
         (start-time (get-internal-real-time))
         (mavg 0)
         (n 1000))
     #'(lambda (sock data)
         (let ((current (babel:octets-to-string data)))
           (unless (gethash sock sockets)
             (setf (gethash sock sockets) ""))
                       
           (setf (gethash sock sockets) (concatenate 'string (gethash sock sockets) current))
           (when (position #\} (gethash sock sockets))
             (handler-case 
                 (let* ((req (gethash sock sockets))
                        (pos= (position #\= req))
                        (pos-H (position #\H req))
                        (delay (subseq req (+ pos= 1) (- pos-H 1)))
                        (task-first (+ 6 (search "ref" req)))
                        (task-last (position #\" req :start task-first))
                        (task-id (subseq req task-first task-last))
                        (router-first (+ 12 (search "routerRef" req)))
                        (router-last (position #\" req :start router-first))
                        (router-id (subseq req router-first router-last)))
                   (when (zerop (rem requests 18))
                     (format t "~%" ))
                   (setf mavg (/ (+ (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)
                                    (* n mavg))
                                 (+ n 1))
                         start-time (get-internal-real-time)
                         requests (1+ requests))
                   (format t "~8,3F" (if (zerop mavg) 0 (/ 1 mavg)))
                   ;;(incf server-reqs)
                   ;;(setf server-data (concat server-data (babel:octets-to-string data)))
                   (remhash sock sockets)
                   (as:write-socket-data sock "HTTP/1.1 204 OK
Connection: Close

"
                                         :write-cb (lambda (socket)
                                                     (as:close-socket socket)
                                                     (as:delay #'(lambda()
                                                                   (bb:catcher
                                                                    (bb:multiple-promise-bind (body status headers)
                                                                        (das:http-request (format nil "http://192.168.1.197:8090/comms-router-web/api/routers/~A/tasks/~A" router-id task-id) 
                                                                                          :method :POST
                                                                                          :content-type "application/json"
                                                                                          :content "{\"state\":\"completed\"}")
                                                                      (let((resp (if (stringp body) body (babel:octets-to-string body))))
                                                                        (unless (= status 204)
                                                                          (format t "Status: ~a~%" status)
                                                                          (format t "Headers: ~s~%" headers)
                                                                          (format t "Body: ~a~%" resp))))
                                                                    (das:http-eof ()
                                                                                  (format t "Server hung up unexpectedly =[~%"))
                                                                    (error (e) (format t "Error: ~a~%" e))))
                                                               :time (parse-integer delay)))))
               (error (e) (format t "~S~%~S~%" e (gethash sock sockets)))) ))))
                 :connect-cb (lambda (sock)
                               (declare (ignore sock))
                               ;;(incf connect-num)
                               )))
(as:start-event-loop #'server)


