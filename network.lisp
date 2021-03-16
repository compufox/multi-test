(in-package :multi-test)

(defparameter *remote-hosts* (make-hash-table :test 'eql))
(defparameter *connections* (make-hash-table :test 'equalp))

(defun recv-server (data)
  (multiple-value-bind (set-id id pos) (unpack data)
    (unless (zerop id)
      (setf (gethash id *remote-hosts*) pos))))

(defun recv-client (data)
  (multiple-value-bind (set-id id pos) (unpack data)
    (unless (zerop id)
      (if (and set-id (zerop *pid*))
          (setf *pid* id)
          (setf (gethash id *remote-hosts*) pos)))))

(defun send (socket connections)
  (loop :for c :in connections
        :do (loop :for d :in connections
                  :do (let ((con-num (first c))
                            (host (second d))
                            (port (third d)))
                        (usocket:socket-send socket (pack con-num
                                                          :v (gethash con-num *remote-hosts* (vec2 0 0)))
                                             nil
                                             :host host
                                             :port port)))))
                                           
          

(defun spawn-server (port recv-fn send-fn &key (buffer-size 32))
  "starts server on PORT, calling RECV-FN with the data it recieves, calling SEND-FN to get data to send back

RECV-FN a function that accepts 2 parameters, the data that came in (of type (simple-array (unsigned-byte 8))) and the address of the remote socket
SEND-FN a function that accepts 4 parameters, the server socket, the remote host, the remote port, and the buffer size for the received data

returns a function that will stop the server"
  (declare (type function recv-fn send-fn)
           (type integer port))
  (let* ((server (usocket:socket-connect nil nil
                                         :protocol :datagram
                                         :element-type '(unsigned-byte 8)
                                         :local-host "0.0.0.0"
                                         :local-port port)))
    (-> (loop :while server
              :do (let ((connection-number (1+ (length (alexandria:hash-table-keys *connections*)))))
                    (multiple-value-bind (buffer size remote-host remote-port)
                        (usocket:socket-receive server nil buffer-size)
                      (let ((key (keyify remote-host remote-port)))
                        (unless (gethash key *connections*)
                          (setf (gethash key *connections*)
                                (list connection-number remote-host remote-port))
                          (usocket:socket-send server (pack connection-number :set-id t)
                                               size
                                               :port remote-port
                                               :host remote-host)))
                      (funcall recv-fn buffer)))))
    (-> (loop :while server
              :do (funcall send-fn server (alexandria:hash-table-values *connections*))))
    #'(lambda ()
        (usocket:socket-close server)
        (setf server nil))))

(defun create-client (host port recv-fn &key (buffer-size 32))
  "creates a client socket, connecting to PORT on HOST, calling RECV-FN on data it recieves

returns two functions
first one that allows you to send data over the socket
second one that allows you to close the socket"
  (let ((socket (usocket:socket-connect host port :protocol :datagram
                                                  :element-type '(unsigned-byte 8))))
    (-> (loop :while socket
              :do (funcall recv-fn (usocket:socket-receive socket nil buffer-size))))
    (values #'(lambda (data)
                (let ((buff (make-array buffer-size :element-type '(unsigned-byte 8)
                                                    :initial-element 0)))
                  (when socket
                    (usocket:socket-send socket (replace buff data) buffer-size))))
            #'(lambda ()
                (usocket:socket-close socket)
                (setf socket nil)))))
